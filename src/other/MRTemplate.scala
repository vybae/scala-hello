package other

//import java.util._
//import java.io.IOException
//import java.lang.reflect.ParameterizedType
//import scala.reflect.runtime.universe._
//                
//import org.apache.hadoop.fs.Path
//import org.apache.hadoop.conf._
//import org.apache.hadoop.io._
//import org.apache.hadoop.mapred._
//import org.apache.hadoop.util._
//
///*
// * it is impossible to realize, cause to inner class's actual generic types cannot be assigned
// */
//abstract class MRTemplate[MapKeyOut, MapValueOut, ReduceKeyOut, ReduceValueOut] {
//  
//  class Map extends MapReduceBase with Mapper[LongWritable, Text, MapKeyOut, MapValueOut] {
//    override def map(x$1: LongWritable, x$2: Text, 
//        x$3: OutputCollector[MapKeyOut, MapValueOut], x$4: Reporter) = tmap _
//  }
//  class Reduce extends MapReduceBase with Reducer[MapKeyOut, MapValueOut, ReduceKeyOut, ReduceValueOut] {
//    override def reduce(x$1: MapKeyOut, x$2: java.util.Iterator[MapValueOut], 
//        x$3: OutputCollector[ReduceKeyOut, ReduceValueOut], x$4: Reporter) = treduce _
//  }
//  
//  def tmap(key: LongWritable, value: Text, 
//      out: OutputCollector[MapKeyOut, MapValueOut], rep: Reporter): Unit
//  def treduce(key:MapKeyOut, values:java.util.Iterator[MapValueOut], 
//      out: OutputCollector[ReduceKeyOut, ReduceValueOut], rep: Reporter): Unit
//      
//  def start(jar:Class[_], pathout:String)  {
//		val conf = new JobConf(this.getClass)
//    conf.setJobName(jar.getName)
//    conf.setMapperClass(classOf[Map])
//    conf.setCombinerClass(classOf[Reduce])
//    conf.setReducerClass(classOf[Reduce])
//    
//    // map output class, reduce output class
//    val rfs = jar.getGenericSuperclass.asInstanceOf[ParameterizedType].getActualTypeArguments
//    conf.setMapOutputKeyClass(rfs(0).asInstanceOf[Class[_]])
//    conf.setMapOutputValueClass(rfs(1).asInstanceOf[Class[_]])
//    conf.setOutputKeyClass(rfs(2).asInstanceOf[Class[_]])
//    conf.setOutputValueClass(rfs(3).asInstanceOf[Class[_]])
//    
//    conf.setInputFormat(classOf[TextInputFormat])
//    conf.setOutputFormat(classOf[TextOutputFormat[ReduceKeyOut, ReduceValueOut]])
//		// set input path
//		FileInputFormat.setInputPaths(conf, new Path("/input"))
//		// set output path
//		FileOutputFormat.setOutputPath(conf, new Path("/output/"+pathout))
//		
//    JobClient.runJob(conf)
//	}
//}
//
