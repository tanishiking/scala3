package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.semanticdb.{javalite as jl}
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.core.Contexts.Context
import scala.annotation.internal.sharable

object DiagnosticOps:
  @sharable private val asciiColorCodes = "\u001B\\[[;\\d]*m".r
  extension (d: Diagnostic)
    def toSemanticDiagnostic: jl.Diagnostic =
      val severity = d.level match
        case ERROR => jl.Diagnostic.Severity.ERROR
        case WARNING => jl.Diagnostic.Severity.WARNING
        case INFO => jl.Diagnostic.Severity.INFORMATION
        case _ => jl.Diagnostic.Severity.INFORMATION
      val msg = asciiColorCodes.replaceAllIn(d.msg.message, m => "")
      val b = jl.Diagnostic
        .newBuilder()
        .setSeverity(severity)
        .setMessage(msg)
      Scala3.range(d.pos.span, d.pos.source).foreach(r => b.setRange(r))
      b.build()
