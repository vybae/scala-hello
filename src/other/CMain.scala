package other
//import org.apache.hadoop.fs.Path
//import org.apache.hadoop.conf._
//import org.apache.hadoop.io._
//import org.apache.hadoop.mapred._
//import org.apache.hadoop.util._
//
//import java.util._
//import java.lang.reflect.ParameterizedType
//
//abstract class CMain {
//  def start(map: Class[_ <: Mapper[_, _, _, _]], reduce: Class[_ <: Reducer[_, _, _, _]], pathout: String): Unit = {
//    val conf = new JobConf(this.getClass)
//    conf.setJobName("ddd")
//    conf.setMapperClass(map)
//    conf.setCombinerClass(reduce)
//    conf.setReducerClass(reduce)
//
//    // map output class, reduce output class
//    var rfs = reduce.getGenericInterfaces()(0).asInstanceOf[ParameterizedType].getActualTypeArguments
//    conf.setMapOutputKeyClass(rfs(0).asInstanceOf[Class[_]])
//    conf.setMapOutputValueClass(rfs(1).asInstanceOf[Class[_]])
//    conf.setOutputKeyClass(rfs(2).asInstanceOf[Class[_]])
//    conf.setOutputValueClass(rfs(3).asInstanceOf[Class[_]])
//
//    conf.setInputFormat(classOf[TextInputFormat])
//    conf.setOutputFormat(classOf[TextOutputFormat[_, _]])
//    // set input path
//    FileInputFormat.setInputPaths(conf, new Path("/input"))
//    // set output path
//    FileOutputFormat.setOutputPath(conf, new Path(pathout))
//
//    JobClient.runJob(conf)
//  }
//}