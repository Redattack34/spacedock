package data.xml

import com.codecommit.antixml._
import com.codecommit.antixml.XML
import java.io.File
import scala.collection.immutable.HashMap

object Localization {
  case class Token( index: Int, text: String )

  private def tokens(e: Elem) : Seq[Token] = for {
    tokenList <- e \ 'TokenList
    token <- tokenList \ 'Token
    index <- token \ 'Index \ text
    string <- token \ 'Text \ text
  } yield Token( index.toInt, string )

  def loadTokens( f: File ) : Map[Int, String] = {
    val allTokens = tokens(XML.fromInputStream(XmlUtils.read(f))).map( token => (token.index, token.text))
    allTokens.toMap
  }

  def main(args: Array[String]) {
    val f = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive\\Content\\Localization")
    val allHulls = for {
      file <- f.listFiles()
      xml = XML.fromInputStream(XmlUtils.read(file))
      tokenList = tokens(xml)
    } yield (file.getName, tokenList)

    allHulls.filter(_._2.isEmpty).foreach(f => println("Failed to parse: " + f._1))
  }
}