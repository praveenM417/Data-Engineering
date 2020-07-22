package com.letterFrequency.hd.wc;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.Reducer.Context;
import org.apache.hadoop.mapreduce.lib.output.MultipleOutputs;

import textPairKey.TextPair;

public class LetterReducer extends Reducer <TextPair, IntWritable, Text, IntWritable>
{   // Counter for Digits Occurence
	public enum numbers {
		DIGITS_OCCURANCE
		}
	
	private IntWritable result = new IntWritable();
	/* Initialising textpair */
	private static TextPair mapOutputKey1 = new TextPair();
	/* Initialising multipleOutputs inside setup */
	private MultipleOutputs<Text, IntWritable> multipleOutputs;

	protected void setup(Context context) throws IOException, InterruptedException {
		multipleOutputs = new MultipleOutputs<Text, IntWritable>(context);
	}

	public void reduce(TextPair key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException
	{
		int count_ = 0;
		char c = key.getSecond().toString().toLowerCase().charAt(0); 
		char d = key.getSecond().toString().charAt(0); 
		String regex = "[0-9]+";
		String NotNumber = "^[0-9]+";
		for (IntWritable record : values)
		{   
			// Incrementing counters for digit occurrence
			if(key.getSecond().toString().matches(regex))
		    context.getCounter(numbers.DIGITS_OCCURANCE).increment(1);
		    count_ = count_+record.get();
				
		}
		if((c >= 'a' && c <= 'z'))
		{ // Giving the language name as the prefix to the output file using multipleoutputs
		String basePath = key.getFirst().toString()+"_out_";
		multipleOutputs.write(new Text(key.getFirst().toString()+";"+key.getSecond().toString()), new IntWritable(count_), basePath);
		}	
		}
	

	protected void cleanup(Context context) throws IOException, InterruptedException {
		//Terminating multipleOutouts
		multipleOutputs.close();
}
}
