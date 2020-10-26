package com.ge.research.sadl.prolog.fileinterface;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class FileInterface{
	public static synchronized void writeFile(String fileName, String text, boolean append){
		//System.out.println("Trying to write: " + text);
		//System.out.println("file name: " + fileName);
		
		try{
    		
    		File file =new File(fileName);
 
    		//if file doesnt exists, then create it
    		if(!file.exists()){
    			file.createNewFile();
    		}
 
    		//true = append file
    		FileWriter fileWritter = new FileWriter(file.getAbsoluteFile(),append);
    	        BufferedWriter bufferWritter = new BufferedWriter(fileWritter);
    	        bufferWritter.write(text);
    	        bufferWritter.close();
 
    	}catch(IOException e){
    		e.printStackTrace();
    	}
	}
	
	public static synchronized Process runFile(String cmd) throws IOException{
		return Runtime.getRuntime().exec(cmd);
	}
}