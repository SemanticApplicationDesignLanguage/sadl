package com.ge.research.sadl.model.visualizer;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;

import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.model.visualizer.IGraphVisualizer.Orientation;
import com.ge.research.sadl.reasoner.ResultSet;

public class TestGraphVizVisualizer {

	@Ignore("https://github.com/crapo/sadlos2/issues/139")
	@Test
	public void test() throws IOException {
		String exec = System.getenv("GraphVizPath");
		assertNotNull(exec);
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
		File cd = new File(".");
		String tempDir = cd.getCanonicalPath() + "/tmp/graphviztest";
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

	@Ignore("https://github.com/crapo/sadlos2/issues/139")
	@Test
	public void testDuplicates() throws IOException {
		String exec = System.getenv("GraphVizPath");
		assertNotNull(exec);
		String[] header = {"head","edge","tail","tail_duplicate"};
		String[][] data = {
				{"LatticeToTree:Person","LatticeToTree:uses","LatticeToTree:Resource","true"},
				{"LatticeToTree:Resource","value","float","true"},
				{"LatticeToTree:Resource","LatticeToTree:uses","LatticeToTree:Resource","true"}
		};
		ResultSet rs = new ResultSet(header, data);
		IGraphVisualizer gv = new GraphVizVisualizer();
		File cd = new File(".");
		String tempDir = cd.getCanonicalPath() + "/tmp/graphviztest";
		File tdf = new File(tempDir);
		tdf.mkdirs();
		String baseFileName = "testGraphVizVisualizerDuplicates";
		String graphName = "test2";
		String anchorNode = "LatticeToTree:Person";
		Orientation orientation = null;
		String description = "this is a test graph";
		gv.initialize(tempDir, baseFileName, graphName, anchorNode, orientation , description);
		gv.graphResultSetData(rs);
	}

}
