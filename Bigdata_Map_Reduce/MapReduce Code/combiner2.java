package com.letterFrequency.hd.wc;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Reducer.Context;
import org.apache.hadoop.mapreduce.lib.output.MultipleOutputs;

import textPairKey.TextPair;

public class combiner2 extends Reducer <TextPair, IntWritable, TextPair, IntWritable>
{   
	
	private IntWritable result = new IntWritable();
	/* Initialising textpair */
	private static TextPair mapOutputKey1 = new TextPair();
	public void reduce(TextPair key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException
	{
		int count_ = 0;
		/* Aggregating the similar keys within mapper and summing the values */
		for (IntWritable record : values)
		{ 
			count_ = count_+record.get();
				
		}
		context.write(key, new IntWritable(count_));
			
		}
	
}
