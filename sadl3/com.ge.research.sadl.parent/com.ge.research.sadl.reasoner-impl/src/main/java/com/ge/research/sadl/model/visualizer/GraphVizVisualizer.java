package com.ge.research.sadl.model.visualizer;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.hp.hpl.jena.vocabulary.OWL;

public class GraphVizVisualizer implements IGraphVisualizer {
	
	private String tempFolder = null;
	private String baseFileName = null;
	private String graphName = null;
	private String anchorNode = null;
	private Orientation orientation = null;
	private String description = null;

	@Override
	public void initialize(String tempDir, String bfn, String graphName, String anchorNode, Orientation orientation, String description) {
		setTempFolder(tempDir);
		setBaseFileName(bfn);
		setGraphName(graphName);
		setAnchorNode(anchorNode);
		setOrientation(orientation);
		setDescription(description);
	}
	@Override
	public List<String> graphCsvData(String csvFilepath) throws IOException {
		throw new IOException("Method graphCsvData(String filepath) not yet implemented");
	}

	@Override
	public List<String> graphCsvData(DataSource ds) throws IOException {
		throw new IOException("Method graphCsvData(DataSource ds) not yet implemented");
	}

	@Override
	public List<String> graphResultSetData(ResultSet rs) throws IOException {
		File dotFile = constructResultSetToDotFile(rs, new File(getTempFolder()), getBaseFileName(), getGraphName(), getAnchorNode(), getDescription(), getOrientation());
		createGraphVizGraph(dotFile.getCanonicalPath());
		return null;
	}

	public static void createGraphVizGraph(String dotfilepath) throws IOException {
		String exec = System.getenv("GraphVizPath");
		if (exec == null) {
			Map<String, String> map = System.getenv();
			Iterator<String> mitr = map.keySet().iterator();
			while (mitr.hasNext()) {
				String key = mitr.next();
				String val = map.get(key);
				System.out.println(key + " -> " + val);
			}
			exec = "C:\\Apps\\graphviz-2.36\\release\\bin";
		}
		if (exec == null || exec.length() == 0) {
			throw new IOException("Unable to find GraphVizPath. Please set GraphVizPath environment variable to the GraphViz bin folder path.");
		}
    	String dotexec = null;
    	if (!exec.endsWith("dotty") && !exec.endsWith("dotty.exe")) {
    		dotexec = exec + File.separator + "dot";
    		exec = exec + File.separator + "dotty";
    	}
		ProcessBuilder pb = new ProcessBuilder(exec, dotfilepath);
		try {
			pb.start();
		} catch (IOException e) {
			throw new IOException("Unable to run GraphViz dotty; is GraphViz installed and on path? (" + e.getMessage() + ")");
		}
		if (dotexec != null) {
			// dot -Tps filename.dot -o outfile.ps
			ProcessBuilder bppng = new ProcessBuilder(dotexec, "-Tpng", dotfilepath,"-o", dotfilepath + ".png");
			try {
				bppng.start();
			} catch (IOException e) {
				throw new IOException("Unable to run GraphViz dot to generate PNG file; is GraphViz path set properly? (" + e.getMessage() + ")");
			}
		}
	}

	/**
	 * Method to create a GraphViz .dot file from a construct ResultSet
	 * @param rs
	 * @param tmpdir
	 * @param bfn -- base file name to use for output file
	 * @param anchorNodeName -- 
	 * @param anchorNodeLabel -- the node which is the anchor (colored differently)
	 * @param description
	 * @param orientation
	 * @return
	 * @throws IOException
	 */
	public static File constructResultSetToDotFile(ResultSet rs, File tmpdir, String bfn, String anchorNodeName, String anchorNodeLabel, String description, Orientation orientation) throws IOException {
		StringBuilder sb = new StringBuilder("digraph g");
		sb.append(anchorNodeName);
		sb.append(" {\n");
		if (orientation != Orientation.TD){
			sb.append("   rankdir=LR\n");
		}
		List<String> nodes = new ArrayList<String>();
		sb.append("    label=\"");
		if (description != null) {
			sb.append(description);
		}
		else {
			sb.append("Construct ");
			sb.append(anchorNodeName + 1);
		}
		sb.append("\";\n    labelloc=top;\n    labeljust=left;\n");
		
		int nothingCount = 0;
		
		while (rs.hasNext()) {
			boolean repeatObjNode; // end of directed edge
			boolean repeatSubjNode;	// start of directed edge
			Object[] row = rs.next();
			String slbl;			// name of start of directed edge
			String olbl;			// name of end of directed edge
			Object s;
			if (row[0] == null || row[1] == null || row[2] == null) {
				continue;
			}
			
			if (row[0].equals(OWL.Nothing.getURI())) {
				s = OWL.Nothing;
				slbl = OWL.Nothing.getLocalName() + nothingCount;
				nothingCount++;
				repeatSubjNode = false;
			}
			else {
				s = rs.getShowNamespaces() ? row[0] : rs.extractLocalName(row[0]);
				if (!nodes.contains(s.toString())) {
					nodes.add(s.toString());
					slbl = "n" + nodes.size();
					repeatSubjNode = false;
				}
				else {
					slbl = "n" + (nodes.indexOf(s.toString()) + 1);
					repeatSubjNode = true;
				}
			}
			Object o;
			if (row[2].equals(OWL.Nothing.getURI())) {
				o = OWL.Nothing;
				olbl = OWL.Nothing.getLocalName() + nothingCount;
				nothingCount++;
				repeatObjNode = false;
			}
			else {
				o = rs.getShowNamespaces() ? row[2] : rs.extractLocalName(row[2]);
				if (!nodes.contains(o.toString())) {
					nodes.add(o.toString());
					olbl = "n" + nodes.size();
					repeatObjNode = false;
				}
				else {
					olbl = "n" + (nodes.indexOf(o.toString()) + 1);
					repeatObjNode = true;
				}
			}
			sb.append("     ");
			if (!repeatSubjNode) {
				sb.append(slbl);
				if(s.equals(OWL.Nothing)) {
					sb.append("[shape=point label=\"");
				}
				else {
					sb.append("[shape=box label=\"");
				}
				sb.append(s.toString());
				sb.append("\"");
				if (anchorNodeLabel != null && s.toString().equals(anchorNodeLabel)) {
					// color the "anchor" node
					sb.append(" color=lightblue style=filled fontcolor=navyblue");
				}
				sb.append("];\n");
			}
			if (!repeatObjNode) {
				sb.append("     ");
				sb.append(olbl);
				if (o.equals(OWL.Nothing)) {
					sb.append("[shape=point label=\"");
				}
				else {
					sb.append("[shape=box label=\"");
				}
				sb.append(o.toString());
				sb.append("\"");
				if (anchorNodeLabel != null && o.toString().equals(anchorNodeLabel)) {
					// color the "anchor" node
					sb.append(" color=lightblue style=filled fontcolor=navyblue");
				}
				sb.append("];\n");
			}
			sb.append("     ");
			sb.append(slbl);
			sb.append("->");
			sb.append(olbl);
			sb.append("[label=\"");
			String edgeLbl = rs.getShowNamespaces() ? row[1].toString() : rs.extractLocalName(row[1]);
			sb.append(edgeLbl);
			sb.append("\"");
			// color the "anchor" edge
			if (anchorNodeLabel != null && edgeLbl.equals(anchorNodeLabel)) {
				sb.append(" color=red");
			}
			sb.append("];\n");
		}
		sb.append("}\n");
		File dotFile = new java.io.File(tmpdir.getAbsolutePath() + File.separator + 
				((bfn != null ? bfn : "") + "Graph.dot"));
		new SadlUtils().stringToFile(dotFile, sb.toString(), false);
		return dotFile;
	}
	private String getTempFolder() {
		return tempFolder;
	}
	private void setTempFolder(String tempFolder) {
		this.tempFolder = tempFolder;
	}
	private String getBaseFileName() {
		return baseFileName;
	}
	private void setBaseFileName(String baseFileName) {
		this.baseFileName = baseFileName;
	}
	private String getAnchorNode() {
		return anchorNode;
	}
	private void setAnchorNode(String anchorNode) {
		this.anchorNode = anchorNode;
	}
	private Orientation getOrientation() {
		return orientation;
	}
	private void setOrientation(Orientation orientation) {
		this.orientation = orientation;
	}
	private String getDescription() {
		return description;
	}
	private void setDescription(String description) {
		this.description = description;
	}
	private String getGraphName() {
		return graphName;
	}
	private void setGraphName(String graphName) {
		this.graphName = graphName;
	}

}
