package data.xml

import java.io.File

import scala.Array.canBuildFrom

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import data.general.FileExtension._

case class Weapon( name: String, weaponType: String, range: Int,
    fireDelay: Float, projectileCount: Option[Int], projectileSpeed: Option[Int],
    beamPowerPerSec: Option[Int],
    ordnancePerShot: Option[Float],
    powerPerShot: Option[Float])

object Weapon extends XmlLoader[Weapon]{

  def load(f: Option[File], e : Elem) : Seq[Weapon] = for {
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
      shotPower.headOption.map(_.toFloat) )

  def directory(base: File) = base / 'Weapons
  
}