package com.letterFrequency.hd.wc;

import java.io.IOException;
import java.net.URI;
import java.util.StringTokenizer;

import org.apache.commons.logging.Log;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.RawLocalFileSystem;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.jobcontrol.ControlledJob;
import org.apache.hadoop.mapreduce.lib.jobcontrol.JobControl;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;
import org.apache.htrace.shaded.commons.logging.LogFactory;

import ListPartitioner.ListPartitioner;
import textPairKey.TextPair;

public class WordCount {
    public static void main(String[] arg0) throws Exception { /*Initialising config for first job*/
        Configuration conf = new Configuration();
        conf.set("mapreduce.output.textoutputformat.separator", ";"); // Setting the seperator of the output file as ';'
        String[] otherArgs = new GenericOptionsParser(conf, arg0).getRemainingArgs();
        if (otherArgs.length < 2) {
            System.err.println("Usage: wordcount <in> [<in>...] <out>");
            System.exit(2);
        }
        /* Job 1 configuration */
        Job job = new Job(conf, "word count");
        job.setJarByClass(WordCount.class);
        job.setMapperClass(TokenizerMapper.class);
        job.setCombinerClass(combiner.class);
        job.setReducerClass(IntSumReducer.class);
        job.setMapOutputKeyClass(TextPair.class);
        job.setMapOutputValueClass(IntWritable.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(IntWritable.class);
        /* input output configuration of mapreduce 1 */
        conf.set("inp_path", arg0[0]);
        FileInputFormat.addInputPath(job, new Path(arg0[0]));
        FileOutputFormat.setOutputPath(job, new Path(arg0[2]));
        /*Initialising config for second job*/
        Configuration conf2 = new Configuration();
        conf2.set("mapreduce.output.textoutputformat.separator", ";");
        /* Job 2 configuration */
        Job job2 = Job.getInstance(conf2);
        job2.setJarByClass(WordCount.class);
        job2.setMapperClass(LetterMapper.class);
        job2.setMapOutputKeyClass(TextPair.class);
        job2.setMapOutputValueClass(IntWritable.class);
        job2.setCombinerClass(combiner2.class);
        job2.setPartitionerClass(ListPartitioner.class); // Cutom partitioner
        job2.setNumReduceTasks(3); // Setting up 3 reducers for 3 files
        job2.setReducerClass(LetterReducer.class);
        job2.setOutputKeyClass(Text.class);
        job2.setOutputValueClass(IntWritable.class);
        /* input output configuration of mapreduce 2 */
        FileInputFormat.addInputPath(job2, new Path(arg0[2]));
        FileOutputFormat.setOutputPath(job2, new Path(arg0[1]));
       // Check if job 1 is successful then proceed to job 2 :: Chaining Mapreduce
        if (job.waitForCompletion(true)) {
        	ControlledJob cJob2 = new ControlledJob(conf2);
            cJob2.setJob(job2);
            // Adding job 2 to the control flow
            JobControl jobctrl = new JobControl("jobctrl");
            jobctrl.addJob(cJob2);
            //cJob2.addDependingJob(cJob1);			  
            jobctrl.run();
            while (!jobctrl.allFinished()) {
                System.out.println("Still running...");
                Thread.sleep(5000);
            }
            System.out.println("done");
            jobctrl.stop();
        	}

        System.exit(1);
    }
}
class JobRunner implements Runnable {
    private JobControl control;

    public JobRunner(JobControl _control) {
        this.control = _control;
    }

    public void run() {
        this.control.run();
    }
}