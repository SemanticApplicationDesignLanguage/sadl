package com.ge.research.sadl.model.visualizer;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.hp.hpl.jena.vocabulary.OWL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GraphVizVisualizer implements IGraphVisualizer {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(GraphVizVisualizer.class);
	private String tempFolder = null;
	private String baseFileName = null;
	private String graphName = null;
	private String anchorNode = null;
	private Orientation orientation = null;
	private String description = null;
	
	private String graphFileToOpen = null;
	private int nothingCount;
	private HashMap<String,String> graphedSubjectMap;
	private HashMap<String,String> duplicateObjectMap;
	private StringBuilder sb;
	private boolean repeatObjNode;
	private boolean repeatSubjNode;
	private List<String> nodes;
	private HashMap<Object, List<Object[]>> subjectList;
	private HashMap<Object, List<Object[]>> objectList;

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

	public void createGraphVizGraph(String dotfilepath) throws IOException {
		String exec = System.getenv("GraphVizPath");
		if (exec == null) {
			Map<String, String> map = System.getenv();
			Iterator<String> mitr = map.keySet().iterator();
			while (mitr.hasNext()) {
				String key = mitr.next();
				String val = map.get(key);
				LOGGER.debug(key + " -> " + val);
			}
		}
		if (exec == null || exec.length() == 0) {
			throw new IOException("Unable to find GraphVizPath. Please set GraphVizPath environment variable to the GraphViz bin folder path.");
		}
    	String dotexec = null;
    	if (!exec.endsWith("dotty") && !exec.endsWith("dotty.exe")) {
    		dotexec = exec + File.separator + "dot";
    		exec = exec + File.separator + "dotty";
    	}
		if (dotexec != null) {
			// dot -Tps filename.dot -o outfile.ps
			String svgfilepath = dotfilepath;
			if (dotfilepath.endsWith(".dot")) {
				svgfilepath = dotfilepath.substring(0, dotfilepath.length() - 4);
			}
			String svgfn = svgfilepath + getGraphFilenameExtension();
			File f = new File(svgfn);
			if (f.exists()) {
				boolean status = f.delete();
				if (!status) {
					throw new IOException("Unable to delete existing file '" + svgfn + "'.");
				}
			}
			ProcessBuilder bppng = new ProcessBuilder(dotexec, "-Tsvg", dotfilepath,"-o", svgfn);
			try {
				bppng.start();
				Thread.sleep(100);
				graphFileToOpen = svgfn;
			} catch (IOException e) {
				throw new IOException("Unable to run GraphViz dot to generate PNG file; is GraphViz path set properly? (" + e.getMessage() + ")");
			} catch (InterruptedException e) {
				LOGGER.error("Ignoring " + e, e);
			}
			int cntr = 0;
			File fto = new File(graphFileToOpen);
			while (cntr++ < 50 && !fto.exists()) {
				try {
					Thread.sleep(200);
				} catch (InterruptedException e) {
					LOGGER.error("Ignoring " + e, e);
				}
			}
			if (!fto.exists()) {
				ProcessBuilder pb = new ProcessBuilder(exec, dotfilepath);
				try {
					pb.start();
				} catch (IOException e2) {
					throw new IOException("Unable to run GraphViz dotty; is GraphViz installed and on path? (" + e2.getMessage() + ")");
				}
			}
			else {
				String dontdelete = System.getenv("GraphVizKeepDot");
				if (dontdelete == null || !dontdelete.equalsIgnoreCase("true")) {
					// delete the .dot file
					File dotfile = new File(dotfilepath);
					if (dotfile.exists()) {
						dotfile.delete();
					}
				}
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
	public File constructResultSetToDotFile(ResultSet rs, File tmpdir, String bfn, String anchorNodeName, String anchorNodeLabel, String description, Orientation orientation) throws IOException {
		Map<Integer,String> headAttributes = null;
		Map<Integer,String> edgeAttributes = null;
		Map<Integer,String> tailAttributes = null;
		if (rs.getColumnCount() > 3) {
			String[] headers = rs.getColumnNames();
			String headName = headers[0];
			String edgeName = headers[1];
			String tailName = headers[2];
			for (int i = 3; i < headers.length; i++) {
				String attrHeader = headers[i];
				if (attrHeader.startsWith(headName)) {
					headAttributes = addAttribute(headAttributes, headName, i, attrHeader);
				}
				else if (attrHeader.startsWith(edgeName)) {
					edgeAttributes = addAttribute(edgeAttributes, edgeName, i, attrHeader);
				}
				else if (attrHeader.startsWith(tailName)) {
					tailAttributes = addAttribute(tailAttributes, tailName, i, attrHeader);
				}
				else {
					throw new IOException("Column " + (i + 1) + " header '" + headers[i] + "' does not begin with a valid identifier from the first 3 headers: '" + headName + "' or '" + edgeName + "' or '" + tailName + "')");
				}
			}
			
		}
		sb = new StringBuilder("digraph g");
		sb.append(anchorNodeName);
		sb.append(" {\n");
		if (orientation != null && orientation != Orientation.TD){
			sb.append("   rankdir=");
			sb.append(orientation.toString());
			sb.append("\n");
		}
		nodes = new ArrayList<String>();
		sb.append("    label=\"");
		if (description != null) {
			sb.append(description);
		}
		else {
			sb.append("Construct ");
			sb.append(anchorNodeName + 1);
		}
		sb.append("\";\n    labelloc=top;\n    labeljust=left;\n");
		
		nothingCount = 0;
		graphedSubjectMap = null;
		duplicateObjectMap = null;
		
		subjectList = new HashMap<Object, List<Object[]>>();
		objectList = new HashMap<Object, List<Object[]>>();
		while (rs.hasNext()) {
			Object[] row = rs.next();
			if (row[0] != null) {
				addRowToMap(subjectList, rs.getShowNamespaces() ? row[0] : rs.extractLocalName(row[0]), row);
//				subjectList.add(rs.getShowNamespaces() ? row[0] : rs.extractLocalName(row[0]));
			}
			if (row[2] != null) {
				addRowToMap(objectList, rs.getShowNamespaces() ? row[2] : rs.extractLocalName(row[2]), row);
//				objectList.add(rs.getShowNamespaces() ? row[2] : rs.extractLocalName(row[2]));
			}
		}
		rs.first();	// reset cursor
		
		while (rs.hasNext()) {
			Object[] row = rs.next();
			String slbl;			// name of start of directed edge
			Object s;
			//If this is a single node to be graphed
			if(row[0] != null && row[1] == null && row[2] == null){
				//is a single node
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
					boolean anchored = false;
					if (anchorNodeLabel != null && s.toString().equals(anchorNodeLabel)) {
						anchored = true;
					}
					applyAttributesToNode(headAttributes, row, anchored);
					sb.append("];\n");
				}

				continue;
				
			}else if (row[0] == null && row[1] == null && row[2] == null) {
				continue;
			}
			
			s = rs.getShowNamespaces() ? row[0] : rs.extractLocalName(row[0]);
			String edgeLbl = rs.getShowNamespaces() ? row[1].toString() : rs.extractLocalName(row[1]);
			Object o = rs.getShowNamespaces() ? row[2] : rs.extractLocalName(row[2]);

			createGraphTriple(s, edgeLbl, o, headAttributes, edgeAttributes, tailAttributes, row, anchorNodeLabel, null);
		}
		if (duplicateObjectMap != null) {
			Iterator<String> objitr = duplicateObjectMap.keySet().iterator();
			while (objitr.hasNext()) {
				String key = objitr.next();
				String objnm = duplicateObjectMap.get(key);
				if (graphedSubjectMap != null && graphedSubjectMap.containsValue(objnm) && !graphedSubjectMap.containsKey(key)) {
					// we need to fill out missing branches
					rs.first();
					while (rs.hasNext()) {
						Object[] row = rs.next();
						if (row[0].equals(objnm)) {
							Object s = rs.getShowNamespaces() ? row[0] : rs.extractLocalName(row[0]);
							String edgeLbl = rs.getShowNamespaces() ? row[1].toString() : rs.extractLocalName(row[1]);
							Object o = rs.getShowNamespaces() ? row[2] : rs.extractLocalName(row[2]);
							createGraphTriple(s, edgeLbl, o, headAttributes, edgeAttributes, tailAttributes, row, anchorNodeLabel, key);
						}
					}
				}
			}
		}
		sb.append("}\n");
		File dotFile = new java.io.File(tmpdir.getAbsolutePath() + File.separator + 
				((bfn != null ? bfn : "") + ".dot"));
		new SadlUtils().stringToFile(dotFile, sb.toString(), false);
		return dotFile;
	}
	
	private void addRowToMap(HashMap<Object, List<Object[]>> list, Object key, Object[] row) {
		if (list.containsKey(key)) {
			list.get(key).add(row);
		}
		else {
			List<Object[]> valList = new ArrayList<Object[]>();
			valList.add(row);
			list.put(key, valList);
		}	
	}

	private void createGraphTriple(Object s, String edgeLbl, Object o, Map<Integer, String> headAttributes, Map<Integer, String> edgeAttributes, Map<Integer, 
			String> tailAttributes, Object[] row, String anchorNodeLabel, String subjectNode) {
		String slbl = subjectNode;
		String olbl;
		if (slbl == null) {
			if (row[0].equals(OWL.Nothing.getURI())) {
				s = OWL.Nothing;
				slbl = OWL.Nothing.getLocalName() + nothingCount;
				nothingCount++;
				repeatSubjNode = false;
			}
			else {
				//check if this node should be duplicated: Used in graphing context AATIM-1389
				boolean duplicateNode = getDuplicateAttribute(headAttributes, row);
				long headSequenceNum = getSequenceNumber(headAttributes, row);
				if (duplicateNode || headSequenceNum > 0) {
					//don't check to see if this node is in nodes
					nodes.add(s.toString());
					if (headSequenceNum > 0) {
						slbl = "n" + headSequenceNum;
					}
					else {
						slbl = "n" + nodes.size();
					}
					repeatSubjNode = false;
				}else if (!nodes.contains(s.toString())) {
					nodes.add(s.toString());
					slbl = "n" + nodes.size();
					repeatSubjNode = false;
				}
				else {
					slbl = "n" + (nodes.indexOf(s.toString()) + 1);
					repeatSubjNode = true;
				}
				if (graphedSubjectMap == null) {
					graphedSubjectMap = new HashMap<String,String>();
				}
				graphedSubjectMap.put(slbl, s.toString());
			}
		}
		else {
			repeatSubjNode = true;
		}
		if (row[2] != null && row[2].equals(OWL.Nothing.getURI())) {
			o = OWL.Nothing;
			olbl = OWL.Nothing.getLocalName() + nothingCount;
			nothingCount++;
			repeatObjNode = false;
		}
		else {
			//check if this node should be duplicated: Used in graphing context AATIM-1389
			boolean duplicateNode = getDuplicateAttribute(tailAttributes, row);
			long tailSequenceNum = getSequenceNumber(tailAttributes, row);
			if(duplicateNode || tailSequenceNum > 0){
				//don't check to see if this node is in nodes
				nodes.add(o.toString());
				if (tailSequenceNum > 0) {
					olbl = "n" + tailSequenceNum;
				}
				else {
					olbl = "n" + nodes.size();
				}
				repeatObjNode = false;
				if (subjectNode == null && repeatSubjNode) {
					// now we want to expand out any children that this duplicate should have so it continues what's in the data...
					if (duplicateObjectMap == null) {
						duplicateObjectMap = new HashMap<String,String>();
					}
					duplicateObjectMap.put(olbl, o.toString());
				}
			}else if (!nodes.contains(o.toString())) {
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
			sb.append(replaceDoubleQuotes(s.toString()));
			sb.append("\"");
			boolean anchored = false;
			if (anchorNodeLabel != null && s.toString().equals(anchorNodeLabel)) {
				anchored = true;
			}
			applyAttributesToNode(headAttributes, row, anchored);
			if (objectList.containsKey(s)) {
				List<Object[]> valList = objectList.get(s);
				for (int i = 0; i < valList.size(); i++) {
					applyAttributesToNode(tailAttributes, valList.get(i), anchored);
				}
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
			sb.append(replaceDoubleQuotes(o.toString()));
			sb.append("\"");
			boolean anchored = false;
			if (anchorNodeLabel != null && o.toString().equals(anchorNodeLabel)) {
				anchored = true;
			}
			applyAttributesToNode(tailAttributes, row, anchored);
			if (subjectList.containsKey(o)) {
				List<Object[]> valList = subjectList.get(o);
				for (int i = 0; i < valList.size(); i++) {
					applyAttributesToNode(headAttributes, valList.get(i), anchored);
				}
			}
			sb.append("];\n");
		}
		sb.append("     ");
		sb.append(slbl);
		sb.append("->");
		sb.append(olbl);
		sb.append("[");
		boolean anchored = false;
		if (row[1] != null && row[1].toString().length() > 0) {
			if (anchorNodeLabel != null && edgeLbl.equals(anchorNodeLabel)) {
				anchored = true;
			}
			sb.append("label=\"");
			sb.append(replaceDoubleQuotes(edgeLbl));
			sb.append("\"");
			// color the "anchor" edge
			if (anchored) {
				sb.append(" style=bold");
			}
		}
		if (edgeAttributes != null) {
			Iterator<Integer> itr = edgeAttributes.keySet().iterator();
			while (itr.hasNext()) {
				Integer key = itr.next();
				String value = edgeAttributes.get(key);
				if (anchored && value.equals("style")) {
					continue; // already set
				}
				if (row[key.intValue()] != null) {
					sb.append(" ");
					sb.append(value);
					sb.append("=");
					sb.append(row[key.intValue()]);
				}
			}
		}
		sb.append("];\n");
	}

	/**
	 * Label strings are double quoted, so they cannot contain double quotes or dot file will be invalid
	 * @param lbl
	 * @return
	 */
	private Object replaceDoubleQuotes(String lbl) {
		if (lbl.contains("\"")) {
			lbl = lbl.replace('\"', '\'');
		}
		return lbl;
	}

	private void applyAttributesToNode(Map<Integer, String> attributes, Object[] row, boolean anchored) {
		boolean anchorFillColorSet = false;
		boolean fontColorSet = false;
		if (attributes != null) {
			Iterator<Integer> itr = attributes.keySet().iterator();
			while (itr.hasNext()) {
				Integer key = itr.next();
				String value = attributes.get(key);		
				if (row[key.intValue()] != null) {
					if (value.equals("fontcolor")) {
						fontColorSet = true;
					}
					sb.append(" ");
					sb.append(value);
					sb.append("=");
					if (anchored && value.equals("fillcolor")) {
						sb.append("\"lightblue:");
						anchorFillColorSet = true;
						sb.append(row[key.intValue()]);
						sb.append("\"");
					}
					else {
						Object rval = row[key.intValue()];
						if (needsQuotes(rval)) {
							rval = escapeQuotes(rval.toString());
							sb.append("\"" + rval + "\"");
						}
						else {
							sb.append(rval);
						}
					}
				}
			}
			if (anchored) {
				if (!anchorFillColorSet) {
					sb.append(" fillcolor=lightblue");
					sb.append(" style=filled");
				}
				if (!fontColorSet) {
					sb.append(" fontcolor=navyblue");
				}
			}
		}
	}
	
	private Object escapeQuotes(String str) {
		if (str.indexOf('\"') > 0) {
			str = str.replace("\"", "\\\"");
		}
		return str;
	}

	/**
	 * Method to detect if an attribute value needs to be quoted.
	 * @param rval
	 * @return
	 */
	private boolean needsQuotes(Object rval) {
		if (rval instanceof String) {
			if (rval.toString().indexOf(" ") >= 0) {
				return true;
			}
			String str = (String)rval;
			for(int i =0; i<str.length(); i++) {
				char ch = str.charAt(i);
				if (Character.isWhitespace(ch) || Character.isISOControl(ch)) {
					return true;
				}
				if (ch == '.'){
					return true;
				} 
				if (ch == '-') {
					return true;
				}
			}		
		}
		return false;
	}

	private long getSequenceNumber(Map<Integer, String> attributes, Object[] row) {
		if (attributes != null && attributes.containsValue("sequence_number")) {
			Iterator<Integer> keyitr = attributes.keySet().iterator();
			while (keyitr.hasNext()) {
				Integer key = keyitr.next();
				String val = attributes.get(key);
				if (val.equals("sequence_number")) {
					Object rowval = row[key.intValue()];
					if (rowval instanceof Long) {
						return (long) rowval;
					}
					return Long.parseLong(rowval.toString());
				}
			}
		}
		return -1;
	}
	
	private boolean getDuplicateAttribute(Map<Integer, String> attributes, Object[] row) {
		if(attributes != null && 
				attributes.containsValue("duplicate")) {
			Iterator<Integer> keyitr = attributes.keySet().iterator();
			while (keyitr.hasNext()) {
				Integer key = keyitr.next();
				String val = attributes.get(key);
				if (val.equals("duplicate")) {
					if(Boolean.parseBoolean((String) row[key.intValue()])) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	private Map<Integer, String> addAttribute(Map<Integer, String> attrMap, String headName, int columnNumber, String attrHeader) {
		String attrName = attrHeader.substring(headName.length() + 1);
		char c = attrHeader.charAt(headName.length());
		if (c == '_') {
			if (attrMap == null) {
				attrMap = new HashMap<Integer, String>();
			}
			attrMap.put(columnNumber, attrName);
		}
		return attrMap;
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
	
	@Override
	public String getGraphFileToOpen() {
		return graphFileToOpen;
	}

	@Override
	public String getGraphFilenameExtension() {
		return ".svg";
	}
}
