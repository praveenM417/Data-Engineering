package com.letterFrequency.hd.wc;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.util.StringTokenizer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Mapper.Context;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;

import textPairKey.TextPair;

public class LetterMapper extends Mapper<LongWritable, Text, TextPair, IntWritable> {
	/* Initialising textpair */
	private static TextPair mapOutputKey = new TextPair();
	String inputFileLanguage;
	String cacheContent;
	public void map(LongWritable lineOffset, Text record, Context context) throws IOException, InterruptedException {
		
		System.out.println("cache:" + cacheContent);
		
		// Getting the Word part alone
		String word = record.toString().split(";")[1];

		//Iterating through the length of the word to get each character of the word and pass it to reducer
		for (int i = 0; i < word.length(); i++) {
			mapOutputKey.setFirst(record.toString().split(";")[0]); // Language
			mapOutputKey.setSecond(String.valueOf(word.charAt(i)));// Getting the letter of each word
			context.write(mapOutputKey, new IntWritable(Integer.valueOf(record.toString().split(";")[2])));// English A 100 (sample)

		}


	}
}