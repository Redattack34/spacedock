package data.xml

import java.io.File

import scala.Array.canBuildFrom

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text

case class Weapon( name: String, weaponType: String, range: Int,
		fireDelay: Float, projectileCount: Option[Int], projectileSpeed: Option[Int],
		beamPowerPerSec: Option[Int],
		ordnancePerShot: Option[Float],
		powerPerShot: Option[Int])

object Weapon {

  private def weapons(e : Elem) : Seq[Weapon] = for {
    name <- e \ 'UID \ text
    weaponType <- e \ 'WeaponType \ text
    range <- e \ 'Range \ text
    fireDelay <- e \ 'fireDelay \ text

    projectiles = e \ 'ProjectileCount \ text
    bulletSpeed = e \ 'ProjectileSpeed \ text

    beamPower = e \ 'BeamPowerCostPerSecond \ text
    ordnance = e \ 'OrdnanceRequiredToFire \ text
    shotPower = e \ 'PowerRequiredToFire \ text
  } yield Weapon( name, weaponType, range.toInt, fireDelay.toFloat,
      projectiles.headOption.map(_.toInt),
      bulletSpeed.headOption.map(_.toInt),
      beamPower.headOption.map(_.toInt),
      ordnance.headOption.map(_.toFloat),
      shotPower.headOption.map(_.toInt) )

  def loadWeapons( base: File ) : Seq[(File, Option[Weapon])] = {
    val weaponsDir = new File(base.getAbsolutePath() + "/Content/Weapons")
    val allWeapons = for {
      file <- weaponsDir.listFiles().toSeq.par
      xml = XML.fromInputStream(XmlUtils.read(file))
      weapon = weapons(xml)
    } yield (file, weapon.headOption)
    allWeapons.seq
  }

  def main(args: Array[String]) {
    val f = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive\\Content\\Weapons")
    val allWeaps = for {
      file <- f.listFiles()
      xml = XML.fromInputStream(XmlUtils.read(file))
      weap = weapons(xml)
    } yield (file.getName, weap.headOption)

    allWeaps.filter(_._2.isEmpty).foreach(f => println("Failed to parse: " + f._1))
  }
}