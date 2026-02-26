package dotty.tools.dotc.semanticdb

import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.semanticdb.Scala3.given
import dotty.tools.dotc.semanticdb.{javalite as jl}

object Tools:

  /** Converts a Path to a String that is URI encoded, without forcing absolute paths. */
  def mkURIstring(path: Path): String =
    // Calling `.toUri` on a relative path will convert it to absolute. Iteration through its parts instead preserves
    // the resulting URI as relative.
    // To prevent colon `:` from being treated as a scheme separator, prepend a slash `/` to each part to trick the URI
    // parser into treating it as an absolute path, and then strip the spurious leading slash from the final result.
    val uriParts = for part <- path.asScala yield new java.net.URI(null, null, "/" + part.toString, null)
    uriParts.mkString.stripPrefix("/")

  /** Load SemanticDB TextDocument for a single Scala source file
   *
   * @param scalaAbsolutePath Absolute path to a Scala source file.
   * @param scalaRelativePath scalaAbsolutePath relativized by the sourceroot.
   * @param semanticdbAbsolutePath Absolute path to the SemanticDB file.
   */
  def loadTextDocument(
    scalaAbsolutePath: Path,
    scalaRelativePath: Path,
    semanticdbAbsolutePath: Path
  ): jl.TextDocument =
    val reluri  = mkURIstring(scalaRelativePath)
    val sdocs = parseTextDocuments(semanticdbAbsolutePath)
    sdocs.getDocumentsList.asScala.find(_.getUri == reluri) match
    case None => throw new NoSuchElementException(s"$scalaRelativePath")
    case Some(document) =>
      val text = new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8)
      // Assert the SemanticDB payload is in-sync with the contents of the Scala file on disk.
      val md5FingerprintOnDisk = MD5.compute(text)
      if document.getMd5 != md5FingerprintOnDisk then
        throw new IllegalArgumentException(s"stale semanticdb: $scalaRelativePath")
      else
        // Update text document to include full text contents of the file.
        document.toBuilder().setText(text).build()
  end loadTextDocument

  def loadTextDocumentUnsafe(scalaAbsolutePath: Path, semanticdbAbsolutePath: Path): jl.TextDocument =
    val docs = parseTextDocuments(semanticdbAbsolutePath).getDocumentsList
    assert(docs.size() == 1)
    docs.get(0)
      .toBuilder()
      .setText(new String(Files.readAllBytes(scalaAbsolutePath), StandardCharsets.UTF_8))
      .build()

  /** Parses SemanticDB text documents from an absolute path to a `*.semanticdb` file. */
  private def parseTextDocuments(path: Path): jl.TextDocuments =
    val bytes = Files.readAllBytes(path).nn // NOTE: a semanticdb file is a TextDocuments message, not TextDocument
    jl.TextDocuments.parseFrom(bytes)

  def metac(doc: jl.TextDocument, realPath: Path)(using sb: StringBuilder): StringBuilder =
    val symtab = PrinterSymtab.fromTextDocument(doc)
    val symPrinter = SymbolInformationPrinter(symtab)
    val realURI = realPath.toString
    given sourceFile: SourceFile = SourceFile.virtual(doc.getUri, doc.getText)
    val synthPrinter = SyntheticPrinter(symtab, sourceFile)
    sb.append(realURI).nl
    sb.append("-" * realURI.length).nl
    sb.nl
    sb.append("Summary:").nl
    sb.append("Schema => ").append(schemaString(doc.getSchema)).nl
    sb.append("Uri => ").append(doc.getUri).nl
    sb.append("Text => empty").nl
    sb.append("Language => ").append(languageString(doc.getLanguage)).nl
    sb.append("Symbols => ").append(doc.getSymbolsCount).append(" entries").nl
    sb.append("Occurrences => ").append(doc.getOccurrencesCount).append(" entries").nl
    if doc.getDiagnosticsCount > 0 then
      sb.append("Diagnostics => ").append(doc.getDiagnosticsCount).append(" entries").nl
    if doc.getSyntheticsCount > 0 then
      sb.append("Synthetics => ").append(doc.getSyntheticsCount).append(" entries").nl
    sb.nl
    sb.append("Symbols:").nl
    doc.getSymbolsList.asScala.toList.sorted.foreach(s => processSymbol(s, symPrinter))
    sb.nl
    sb.append("Occurrences:").nl
    doc.getOccurrencesList.asScala.toList.sorted.foreach(processOccurrence)
    sb.nl
    if doc.getDiagnosticsCount > 0 then
      sb.append("Diagnostics:").nl
      doc.getDiagnosticsList.asScala.toList.sorted.foreach(d => processDiag(d))
      sb.nl
    if doc.getSyntheticsCount > 0 then
      sb.append("Synthetics:").nl
      doc.getSyntheticsList.asScala.toList.sorted.foreach(s => processSynth(s, synthPrinter))
      sb.nl
    sb
  end metac

  private def schemaString(schema: jl.Schema) =
    schema match
    case jl.Schema.SEMANTICDB3     => "SemanticDB v3"
    case jl.Schema.SEMANTICDB4     => "SemanticDB v4"
    case jl.Schema.LEGACY          => "SemanticDB legacy"
    case jl.Schema.UNRECOGNIZED    => "unknown"
    case _               => "unknown"
  end schemaString

  private def languageString(language: jl.Language) =
    language match
    case jl.Language.SCALA                              => "Scala"
    case jl.Language.JAVA                               => "Java"
    case jl.Language.UNKNOWN_LANGUAGE | jl.Language.UNRECOGNIZED    => "unknown"
    case _                                  => "unknown"
  end languageString

  private def processSymbol(info: jl.SymbolInformation, printer: SymbolInformationPrinter)(using sb: StringBuilder): Unit =
    sb.append(printer.pprintSymbolInformation(info)).nl

  private def processSynth(synth: jl.Synthetic, printer: SyntheticPrinter)(using sb: StringBuilder): Unit =
    sb.append(printer.pprint(synth)).nl

  private def processDiag(d: jl.Diagnostic)(using sb: StringBuilder): Unit =
    if d.hasRange then processRange(sb, d.getRange)
    else sb.append("[):")
    sb.append(" ")
    d.getSeverity match
      case jl.Diagnostic.Severity.ERROR => sb.append("[error]")
      case jl.Diagnostic.Severity.WARNING => sb.append("[warning]")
      case jl.Diagnostic.Severity.INFORMATION => sb.append("[info]")
      case _ => sb.append("[unknown]")
    sb.append(" ")
    sb.append(d.getMessage)
    sb.nl

  private def processOccurrence(occ: jl.SymbolOccurrence)(using sb: StringBuilder, sourceFile: SourceFile): Unit =
    if occ.hasRange then
      val range = occ.getRange
      processRange(sb, range)
      if range.getEndLine == range.getStartLine
      && range.getStartCharacter != range.getEndCharacter
      && !(occ.getSymbol.isConstructor && occ.getRole == jl.SymbolOccurrence.Role.DEFINITION) then
        val line = sourceFile.lineContent(sourceFile.lineToOffset(range.getStartLine))
        assert(range.getStartCharacter <= line.length && range.getEndCharacter <= line.length,
          s"Line is only ${line.length} - start line was ${range.getStartLine} in source ${sourceFile.name}"
        )
        sb.append(" ").append(line.substring(range.getStartCharacter, range.getEndCharacter))
    else
      sb.append("[):")
    sb.append(if occ.getRole == jl.SymbolOccurrence.Role.REFERENCE then " -> " else " <- ").append(occ.getSymbol).nl
  end processOccurrence

  extension (sb: StringBuilder)
    private inline def nl = sb.append(System.lineSeparator)
