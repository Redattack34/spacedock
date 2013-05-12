package data.xml

import java.io.File

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.text

import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension

case class Weapon( name: String, weaponType: String, range: Int,
    fireDelay: Double, isBeam: Boolean, damage: Double, empDamage: Double,
    projectileCount: Option[Int], projectileSpeed: Option[Int],
    salvoCount: Option[Int], salvoTime: Option[Double],
    beamPowerPerSec: Option[Double],
    ordnancePerShot: Option[Double],
    powerPerShot: Option[Double])

object Weapon extends XmlLoader[Weapon]{

  def load(f: Option[File], e : Elem) : Seq[Weapon] = for {
    weaponType <- e \ 'WeaponType \ text
    range <- e \ 'Range \ text
    fireDelay <- e \ 'fireDelay \ text
    isBeam = e \ 'isBeam \ text
    damage <- e \ 'DamageAmount \ text
    empDamage = e \ 'EMPDamage \ text

    projectiles = e \ 'ProjectileCount \ text
    bulletSpeed = e \ 'ProjectileSpeed \ text
    salvoCount = e \ 'SalvoCount \ text
    salvoTimer = e \ 'SalvoTimer \ text

    beamPower = e \ 'BeamPowerCostPerSecond \ text
    ordnance = e \ 'OrdinanceRequiredToFire \ text
    shotPower = e \ 'PowerRequiredToFire \ text
  } yield Weapon( f.get.getName.replace(".xml", ""),
      weaponType, range.toInt, fireDelay.toFloat,
      isBeam.headOption.map(_.toBoolean).getOrElse(false),
      damage.toDouble, empDamage.headOption.map(_.toDouble).getOrElse(0.0d),
      projectiles.headOption.map(_.toInt),
      bulletSpeed.headOption.map(_.toInt),
      salvoCount.headOption.map(_.toInt),
      salvoTimer.headOption.map(_.toDouble),
      beamPower.headOption.map(_.toDouble),
      ordnance.headOption.map(_.toFloat),
      shotPower.headOption.map(_.toFloat) )

  def directory(base: File) = base / 'Weapons

}
