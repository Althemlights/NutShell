import mill._, scalalib._
import coursier.maven.MavenRepository

import $file.`rocket-chip`.common
import $file.`rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build
import $file.`rocket-chip`.hardfloat.build

//millSourcePath默认是./src，可以用override重写
object ivys {
  val sv = "2.12.13"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.0"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.0"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:0.3.2"
  val chiselCirct = ivy"com.sifive::chisel-circt:0.4.0"
  val scalatest = ivy"org.scalatest::scalatest:3.2.2"
  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
}

trait CommonModule extends ScalaModule {

  def chiselOpt: Option[ScalaModule] = None

  override def scalaVersion = "2.12.13"

  override def scalacOptions = Seq("-Xsource:2.11")

  override def ivyDeps = if(chiselOpt.isEmpty) Agg(ivys.chisel3) else Agg.empty[Dep]

  override def scalacPluginIvyDeps = Agg(ivys.macroParadise, ivys.chisel3Plugin)

}

trait HasXsource211 extends ScalaModule {
  override def scalacOptions = T {
    super.scalacOptions() ++ Seq(
      "-deprecation",
      "-unchecked",
      "-Xsource:2.11"
    )
  }
}

trait HasChisel3 extends ScalaModule {
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.0-RC1"
  )
  //override def scalacPluginIvyDeps = Agg(ivy"edu.berkeley.cs:::chisel3-plugin:3.5.0")
}

//SbtModule基本上与普通的 ScalaModule 相同，但被配置为遵循 SBT 项目布局:
/*src
    main/scala
    test/scala  */
//CrossSbtModule 是使用 SBT 项目布局配置的 CrossScalaModule 版本:
//Mill 提供了一个 CrossScalaModule 模板，可以与 Cross 一起跨不同版本的 Scala 交叉构建 Scala 模块。
trait HasChiselTests extends CrossSbtModule  {
  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4", ivy"edu.berkeley.cs::chisel-iotesters:1.2+")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object rocketchip extends `rocket-chip`.common.CommonRocketChip {

  val rcPath = os.pwd / "rocket-chip"

  override def scalaVersion = ivys.sv

  override def scalacOptions = Seq("-Xsource:2.11")

  override def millSourcePath = rcPath

  object configRocket extends `rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
    override def millSourcePath = rcPath / "api-config-chipsalliance" / "design" / "craft"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def pomSettings = T {
      rocketchip.pomSettings()
    }

    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      common.getVersion("chisel3")
    ) else Agg.empty[Dep]
    
    def chisel3PluginIvyDeps = Agg(common.getVersion("chisel3-plugin", cross=true))
  }

  def hardfloatModule = hardfloatRocket

  def configModule = configRocket

}

object huancun extends CommonModule with SbtModule {

  override def millSourcePath = os.pwd / "HuanCun"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )
}

object difftest extends CommonModule with SbtModule {
  override def millSourcePath = os.pwd / "difftest"
}

object fudian extends CommonModule with SbtModule

object chiselModule extends CrossSbtModule with CommonModule with HasChisel3 with HasChiselTests with HasXsource211{

  def crossScalaVersion = "2.12.13"
  override def scalaVersion = "2.12.13"

  //override def compileIvyDeps = Agg(ivys.macroParadise)
  //override def scalacPluginIvyDeps = Agg(ivys.chisel3Plugin, ivys.macroParadise)
  //def chisel3PluginIvyDeps = Agg(ivys.chisel3Plugin, ivys.macroParadise)

  def rocketModule = rocketchip
  def difftestModule = difftest
  def huancunModule = huancun
  def fudianModule = fudian
  
  override def millSourcePath = os.pwd

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    difftestModule,
    huancunModule,
    fudianModule
  )
}
