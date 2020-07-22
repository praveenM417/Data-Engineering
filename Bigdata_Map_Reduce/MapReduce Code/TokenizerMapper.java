package com.letterFrequency.hd.wc;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.util.StringTokenizer;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Mapper.Context;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.commons.io.IOUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import textPairKey.TextPair;
public class TokenizerMapper extends Mapper<LongWritable, Text, TextPair, IntWritable> {
	
	/* Initialising textpair */
	private static TextPair mapOutputKey = new TextPair();
    String inputFileLanguage;
	
    /*USing setup method to find the language of the file*/
	@Override
	protected void setup(Context context) throws IOException, InterruptedException
	{
		FileSplit fileSplit = (FileSplit) context.getInputSplit();
		// getting the split file name using fileSplit API //
		inputFileLanguage = fileSplit.getPath().getName();
		Configuration conf = context.getConfiguration();
		
		/*Setting up the HDFS Config files to open HDFS*/
        conf.addResource(new Path("/etc/hadoop/conf/core-site.xml"));
        conf.addResource(new Path("/etc/hadoop/conf/hdfs-site.xml"));
        
        /* Opening HDFS and scanning the HDFS File*/
        FileSystem fs = FileSystem.get(conf);
        FileStatus[] status = fs.listStatus(new Path("hdfs://nn01.itversity.com:8020/user/praveennm/lf_inp"));
        
        /*Iterating through the files in the input hdfs directory to find the actual split file */
        for (int i = 0; i < status.length; i++) {
            FSDataInputStream inputStream = fs.open(status[i].getPath());
            String content = IOUtils.toString(inputStream, "UTF-8");
            
            /* check if the split file and the file in the hdfs input dir matches, then find the language */
            if(inputFileLanguage.equals(status[i].getPath().toString().split("/")[status[i].getPath().toString().split("/").length-1]))
            inputFileLanguage = content.substring(content.indexOf("Language:"),content.indexOf("***")).replaceAll(" ","").replaceAll("\\n","").replaceAll("\\r","").split(":")[1];
            
        }
	}
	
	public void map(LongWritable lineOffset, Text record, Context context) throws IOException, InterruptedException {
		String s = record.toString().toLowerCase();
		/* getting every word in the split */
		for (String word : s.split("\\W+")) {
		mapOutputKey.setFirst(inputFileLanguage);
		mapOutputKey.setSecond(word);
		
		/* sample output - English Big 100 */
       context.write(mapOutputKey, new IntWritable(1));

	}
	}
}