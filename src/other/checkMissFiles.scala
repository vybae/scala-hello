package other

import scala.io.Source
import java.io.File
import scala.collection.mutable.ArrayBuffer

/*
 * 检查丢失的文件
 */
object checkMissFiles {

  def checkFiles(path: String): Iterable[String] = {
    val arr = new ArrayBuffer[Boolean]()
    set foreach { x => arr += false }
    var map = collection.mutable.Map(set -- ignore zip arr toArray: _*)
    val files = getFiles(new File("""F:\ipex\bin"""))
    files foreach { x => map(x.getName) = true }

    map filter { !_._2 } map { _._1 }
  }

  def getFiles(files: File*): Iterable[File] = {
    val dirs = files.filter { _.isDirectory() }
    if (dirs.size > 0) {
      (Array[File]() /: dirs) {
        (lst, f) => lst ++ getFiles(f.listFiles(): _*)
      }
    }
    else {
      files.filter { x => x.isFile() && x.getName.endsWith(".dll") }
    }
  }

  def main(args: Array[String]): Unit = {
    val files = checkFiles("""F:\ipex3\bin""")
    if (files.size > 0) {
      files foreach println _
    }
    else {
      println("no miss files")
    }
  }
  
  // 忽略的文件
  var ignore = Set[String]()
  ignore += "Dextrys.Utilities.dll"

  // 确保无重复文件
  var set = Set[String]()
  set += "BusinessServiceLibrary.dll"
  set += "Communication.Service.Entity.dll"
  set += "EntityCore.dll"
  set += "PlatFormHelper.dll"
  set += "WebUtility.dll"
  set += "WFFX.Activities.dll"
  set += "WFFX.ActivityService.dll"
  set += "WFFX.Common.dll"
  set += "WFFX.Config.dll"
  set += "WFFX.Consts.dll"
  set += "WFFX.Core.dll"
  set += "WFFX.DA.Business.dll"
  set += "WFFX.DA.Common.dll"
  set += "WFFX.Framework.dll"
  set += "WFFX.Interface.dll"
  set += "WFFX.ServiceComponent.dll"
  set += "WFFX.Utility.dll"
  set += "Workflow.dll"
  set += "AjaxControlToolkit.dll"
  set += "AntiXSSLibrary.dll"
  set += "Castle.Core.dll"
  set += "Castle.DynamicProxy2.dll"
  set += "DataAccessCore.dll"
  set += "EntityCore.dll"
  set += "IPEXCommon.dll"
  set += "itextsharp.dll"
  set += "Jayrock.Json.dll"
  set += "Movitech.Utilities.dll"
  set += "PBusiness.dll"
  set += "PCommon.dll"
  set += "PDataAccess.dll"
  set += "PlatForm.dll"
  set += "PlatFormHelper.dll"
  set += "PResource.dll"
  set += "WebControls.dll"
  set += "WebUtility.dll"
  set += "AjaxControlToolkit.dll"
  set += "AntiXSSLibrary.dll"
  set += "BusinessServiceLibrary.dll"
  set += "Communication.Service.Entity.dll"
  set += "Dextrys.Utilities.dll"
  set += "EntityCore.dll"
  set += "Jayrock.Json.dll"
  set += "PlatFormHelper.dll"
  set += "WebControls.dll"
  set += "WebUtility.dll"
  set += "WFFX.Activities.dll"
  set += "WFFX.Common.dll"
  set += "WFFX.Consts.dll"
  set += "WFFX.Core.dll"
  set += "WFFX.DA.Business.dll"
  set += "WFFX.DA.Common.dll"
  set += "WFFX.Framework.dll"
  set += "WFFX.Interface.dll"
  set += "WFFX.MaintainHelper.dll"
  set += "WorkflowCenter.dll"
  set += "AjaxControlToolkit.dll"
  set += "AntiXSSLibrary.dll"
  set += "Castle.Core.dll"
  set += "Castle.DynamicProxy2.dll"
  set += "Communication.Service.Entity.dll"
  set += "DataAccessCore.dll"
  set += "EntityCore.dll"
  set += "ICSharpCode.SharpZipLib.dll"
  set += "Interop.Shell32.dll"
  set += "IPEXBusiness.dll"
  set += "IPEXCommon.dll"
  set += "IPEXDataAccess.dll"
  set += "IPEXResource.dll"
  set += "itextsharp.dll"
  set += "Jayrock.Json.dll"
  set += "Movitech.Utilities.dll"
  set += "PlatFormHelper.dll"
  set += "POSM.dll"
  set += "WebControls.dll"
  set += "WebUtility.dll"
  set += "Castle.Core.dll"
  set += "Castle.DynamicProxy2.dll"
  set += "DataAccessCore.dll"
  set += "EntityCore.dll"
  set += "ICSharpCode.SharpZipLib.dll"
  set += "Jayrock.Json.dll"
  set += "Movitech.Utilities.dll"
  set += "NPOI.DDF.dll"
  set += "NPOI.dll"
  set += "NPOI.HPSF.dll"
  set += "NPOI.HSSF.dll"
  set += "NPOI.POIFS.dll"
  set += "NPOI.Util.dll"
  set += "PCommon.dll"
  set += "PlatFormHelper.dll"
  set += "RCBusiness.dll"
  set += "RCCommon.dll"
  set += "RCDataAccess.dll"
  set += "ReportCenter.dll"
  set += "RResource.dll"
  set += "SqlAdmin.dll"
  set += "WebControls.dll"
  set += "WebUtility.dll"
  set += "AjaxControlToolkit.dll"
  set += "AntiXSSLibrary.dll"
  set += "Castle.Core.dll"
  set += "Castle.DynamicProxy2.dll"
  set += "Communication.Service.Entity.dll"
  set += "DataAccessCore.dll"
  set += "EntityCore.dll"
  set += "IPEXBusiness.dll"
  set += "IPEXCommon.dll"
  set += "IPEXDataAccess.dll"
  set += "IPEXReport.dll"
  set += "IPEXResource.dll"
  set += "itextsharp.dll"
  set += "Jayrock.Json.dll"
  set += "Movitech.Utilities.dll"
  set += "PlatFormHelper.dll"
  set += "WebControls.dll"
  set += "WebUtility.dll"
  
}