package other

//import java.io.IOException
//import java.util._
//
//import org.apache.hadoop.fs.Path
//import org.apache.hadoop.conf._
//import org.apache.hadoop.io._
//import org.apache.hadoop.mapred._
//import org.apache.hadoop.util._
//import common.CMain
//
//object wc extends CMain {
//  def main(args: Array[String]): Unit = {
//    if (args == null || args.length != 1) {
//      println("please use wc <output>")
//    }
//    start(classOf[wcMap], classOf[wcReduce], args(0))
//  }
//}
//
//class wcMap extends MapReduceBase with Mapper[LongWritable, Text, Text, IntWritable] {
//  private val one = new IntWritable(1);
//  private val word = new Text();
//
//  def map(key: LongWritable, value: Text,
//    output: OutputCollector[Text, IntWritable],
//    reporter: Reporter) {
//
//    val line = value.toString
//    val tokenizer = new StringTokenizer(line)
//    while (tokenizer.hasMoreTokens) {
//      word.set(tokenizer.nextToken)
//      output.collect(word, one)
//    }
//  }
//}
//
//class wcReduce extends MapReduceBase with Reducer[Text, IntWritable, Text, IntWritable] {
//  def reduce(key: Text, values: Iterator[IntWritable],
//    output: OutputCollector[Text, IntWritable],
//    reporter: Reporter) {
//    output.collect(key, new IntWritable(count(0, values)))
//
//    def count(sum: Int, vs: Iterator[IntWritable]): Int =
//      if (vs.hasNext)
//        count(sum + vs.next.get, vs)
//      else
//        sum
//  }
//}