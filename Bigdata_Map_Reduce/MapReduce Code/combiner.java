package com.letterFrequency.hd.wc;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Reducer.Context;

import textPairKey.TextPair;

public class combiner extends Reducer <TextPair, IntWritable, TextPair, IntWritable>
{
	private IntWritable result = new IntWritable();
	/* Initialising seconf text Pair */
	private static TextPair mapOutputKey1 = new TextPair();

	public void reduce(TextPair key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException
	{   /*Aggregation (sum) of counts of similar keys */
		int sum = 0;
		for (IntWritable val : values)
		{
			sum += val.get();
		}
		result.set(sum);
		context.write(key, result);
	}
}
