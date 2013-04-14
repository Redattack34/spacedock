package data.xml

import java.io.File

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text

object Localization {
  private case class Token( index: Int, text: String )

  private def tokens(e: Elem) : Seq[Token] = for {
    tokenList <- e \ 'TokenList
    token <- tokenList \ 'Token
    index <- token \ 'Index \ text
    string <- token \ 'Text \ text
  } yield Token( index.toInt, string )

  def loadTokens( f: File ) : Map[Int, String] = {
    if ( f.exists ) {
      val allTokens = tokens(XML.fromInputStream(XmlUtils.read(f))).map( token => (token.index, token.text))
      allTokens.toMap
    }
    else Map()
  }
}