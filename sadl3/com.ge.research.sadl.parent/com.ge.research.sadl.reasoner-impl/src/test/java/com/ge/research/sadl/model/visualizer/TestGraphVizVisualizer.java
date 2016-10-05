package com.ge.research.sadl.model.visualizer;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation;
import com.ge.research.sadl.reasoner.ResultSet;

public class TestGraphVizVisualizer {

	@Test
	public void test() throws IOException {
		String[] header = {"Head","Edge","Tail","Head_color", "Edge_color", "Tail_color"};
		String[][] data = {
				{"N1","p1","N2","blue","blue","yellow"},
				{"N1","p2","N3","blue","red","orange"},
				{"N3","p3","N4","orange", "black","green"},
				{"N3","p1","N5","orange","blue","magenta"},
				{"N4","p4","N5","green","green","magenta"},
				{"N5","p1","N1","magenta","blue","blue"}};
		ResultSet rs = new ResultSet(header, data);
		IGraphVisualizer gv = new GraphVizVisualizer();
		String tempDir = "c:/tmp/graphviztest";
		File tdf = new File(tempDir);
		tdf.mkdirs();
		String baseFileName = "testGraphVizVisualizer";
		String graphName = "test";
		String anchorNode = "N3";
		Orientation orientation = null;
		String description = "this is a test graph";
		gv.initialize(tempDir, baseFileName, graphName, anchorNode, orientation , description);
		gv.graphResultSetData(rs);
	}

}
