package hellowasm

import scala.scalajs.wit.annotation._
import scala.scalajs.wit
import hellowasm.exports.wasi.cli.Run
import hellowasm.wasi.clocks.wall_clock
import hellowasm.wasi.random.random
import hellowasm.wasi.cli.stdout
import hellowasm.wasi.io.streams.StreamError
import hellowasm.wasi.http.{types => http}
import hellowasm.wasi.http.outgoing_handler

@WitImplementation
object HelloWorld extends Run {
  override def run(): wit.Result[Unit, Unit] = {
    // --- Test pure Wasm library paths (BoxesRunTime, Symbol, Buffer, ArrayBuilder) ---
    testPureWasmLibraryPaths()

    // --- @WitRecord usage: wall_clock.now returns Datetime record ---
    val now = wall_clock.now()
    println(s"Current time: ${now.seconds} seconds, ${now.nanoseconds} nanoseconds")

    // --- @WitImport with unsigned types ---
    val randomBytes = random.getRandomBytes(4L)
    println(s"Random bytes: ${randomBytes.map(_.toInt & 0xFF).mkString(", ")}")

    // --- @WitResourceImport: OutputStream via stdout ---
    val out = stdout.getStdout()
    val message = "Hello from Scala 3 Wasm with WASI!\n".getBytes
    out.blockingWriteAndFlush(message.map(b => b)) match {
      case _: wit.Ok[?] => // success
      case _: wit.Err[?] => println("Failed to write to stdout")
    }

    // --- HTTP request: exercises resource constructors, methods, variants, Result, Optional ---
    println("Making an HTTP GET request to httpbin.org/get ...")

    // Resource constructor: Fields()
    val headers = http.Fields()

    // Resource constructor with arg: OutgoingRequest(headers)
    val request = http.OutgoingRequest(headers)

    // Resource methods with @WitVariant args
    request.setMethod(http.Method.Get)
    request.setScheme(java.util.Optional.of(http.Scheme.Https))
    request.setAuthority(java.util.Optional.of("httpbin.org"))
    request.setPathWithQuery(java.util.Optional.of("/get"))

    // @WitImport: outgoing_handler.handle returns Result[FutureIncomingResponse, ErrorCode]
    val handleResult = outgoing_handler.handle(request, java.util.Optional.empty())
    val futureResponse = handleResult match {
      case ok: wit.Ok[_] => ok.value.asInstanceOf[http.FutureIncomingResponse]
      case err: wit.Err[_] =>
        println(s"Error sending request: ${err.value}")
        return new wit.Err(())
    }

    // Resource methods: subscribe() returns Pollable, block() waits
    val pollable = futureResponse.subscribe()
    pollable.block()

    // Deeply nested: Optional[Result[Result[IncomingResponse, ErrorCode], Unit]]
    val responseOpt = futureResponse.get()
    if (!responseOpt.isPresent()) {
      println("Error: response not ready")
      return new wit.Err(())
    }
    val outerResult = responseOpt.get()
    val innerResult = outerResult match {
      case ok: wit.Ok[_] => ok.value.asInstanceOf[wit.Result[http.IncomingResponse, http.ErrorCode]]
      case _: wit.Err[_] =>
        println("Error: already consumed")
        return new wit.Err(())
    }
    val response = innerResult match {
      case ok: wit.Ok[_] => ok.value.asInstanceOf[http.IncomingResponse]
      case err: wit.Err[_] =>
        println(s"HTTP error: ${err.value}")
        return new wit.Err(())
    }

    // Resource method getter: status() returns UShort (status-code)
    val status = response.status()
    println(s"Status: $status")

    // Resource method: consume() returns Result[IncomingBody, Unit]
    val bodyResult = response.consume()
    val incomingBody = bodyResult match {
      case ok: wit.Ok[_] => ok.value.asInstanceOf[http.IncomingBody]
      case _: wit.Err[_] =>
        println("Error: could not consume body")
        return new wit.Err(())
    }

    // Resource method: stream() returns Result[InputStream, Unit]
    val streamResult = incomingBody.stream()
    val stream = streamResult match {
      case ok: wit.Ok[_] => ok.value.asInstanceOf[http.InputStream]
      case _: wit.Err[_] =>
        println("Error: could not get stream")
        return new wit.Err(())
    }

    // Stream reading loop with blockingRead
    val bodyBuilder = new StringBuilder()
    var done = false
    while (!done) {
      stream.blockingRead(65536L) match {
        case ok: wit.Ok[_] =>
          val bytes = ok.value.asInstanceOf[Array[scala.scalajs.wit.unsigned.UByte]]
          val byteArray = bytes.map(_.toByte)
          bodyBuilder.append(new String(byteArray, "UTF-8"))
        case _: wit.Err[_] =>
          done = true
      }
    }

    println(s"Body:\n${bodyBuilder.toString()}")

    new wit.Ok(())
  }

  /** Exercises library-js code paths guarded by linkTimeIf(targetPureWasm). */
  private def testPureWasmLibraryPaths(): Unit = {
    // --- BoxesRunTime.equals (used by == on boxed types) ---
    val a: Any = 42
    val b: Any = 42
    val c: Any = 43
    assert(a == b, "BoxesRunTime.equals: equal ints")
    assert(!(a == c), "BoxesRunTime.equals: unequal ints")
    val d: Any = Double.NaN
    assert(!(d == d), "BoxesRunTime.equals: NaN != NaN")
    val e: Any = "hello"
    val f: Any = "hello"
    assert(e == f, "BoxesRunTime.equals: equal strings")

    // --- Symbol cache (uses HashMap under pure Wasm instead of js.Dictionary) ---
    val s1 = Symbol("test")
    val s2 = Symbol("test")
    assert(s1 eq s2, "Symbol: interning via pure Wasm cache")

    // --- Buffer (uses ArrayBuffer under pure Wasm instead of js.WrappedArray) ---
    val buf = scala.collection.mutable.Buffer(1, 2, 3)
    buf += 4
    assert(buf.length == 4, "Buffer: basic operations")
    assert(buf(3) == 4, "Buffer: element access")

    val ibuf = scala.collection.mutable.IndexedBuffer(10, 20)
    ibuf += 30
    assert(ibuf.length == 3, "IndexedBuffer: basic operations")

    // --- ArrayBuilder.make (uses linkTimeIf(isWebAssembly)) ---
    val ab = scala.collection.mutable.ArrayBuilder.make[Int]
    ab += 1
    ab += 2
    ab += 3
    val arr = ab.result()
    assert(arr.length == 3, "ArrayBuilder: length")
    assert(arr(0) == 1 && arr(1) == 2 && arr(2) == 3, "ArrayBuilder: elements")

    println("All pure Wasm library path tests passed!")
  }
}
