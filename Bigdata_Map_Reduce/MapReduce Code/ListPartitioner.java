package ListPartitioner;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Partitioner;


import textPairKey.TextPair;

public class ListPartitioner extends Partitioner<TextPair, IntWritable>{
	//Partitioning logic to push each language letter to different reducer
	@Override
	public int getPartition(TextPair arg0, IntWritable arg1, int arg2) {
		if(arg0.getFirst().toString().toLowerCase().contains("english"))
		return 0;
		if(arg0.getFirst().toString().toLowerCase().contains("french"))
		return 1;
		if(arg0.getFirst().toString().toLowerCase().contains("italian"))
		return 2;
		return 3;
		
	}

}
