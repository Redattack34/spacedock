package data.xml

import java.io.File

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import data.general.FileExtension._

object Localization {
  private case class Token( index: Int, text: String )

  private def tokens(e: Elem) : Seq[Token] = for {
    tokenList <- e \ 'TokenList
    token <- tokenList \ 'Token
    index <- token \ 'Index \ text
    string <- token \ 'Text \ text
  } yield Token( index.toInt, string )

  def loadTokens( dir: File, language: String ) : Map[Int, String] = {
    val tokenSeq : Seq[Token] = 
      if ( (dir / 'Localization / (language + ".xml")).exists ) {
        //Old style
        val file = dir / 'Localization / (language + ".xml")
        tokens(XML.fromInputStream(XmlUtils.read(file)))
      }
      else {
        //New Style
        val languageDir : File = dir / 'Localization / language
        val files: Seq[File] = if (languageDir.exists) languageDir.listFiles
                               else Seq()
        val eachFile = for {
          file <- files
        } yield( tokens(XML.fromInputStream(XmlUtils.read(file))))
        eachFile.flatten
      }
    
    
      tokenSeq.map(tok => (tok.index, tok.text)).toMap
  }
}