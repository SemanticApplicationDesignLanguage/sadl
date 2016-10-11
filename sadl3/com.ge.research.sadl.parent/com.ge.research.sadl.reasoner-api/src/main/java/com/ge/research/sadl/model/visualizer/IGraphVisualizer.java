/************************************************************************
 * Copyright \u00a9 2007-2016 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a 
 * language for building semantic models and expressing rules that 
 * capture additional domain knowledge. The SADL-IDE (integrated 
 * development environment) is a set of Eclipse plug-ins that 
 * support the editing and testing of semantic models using the 
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES 
 * and licensed under the Eclipse Public License - v 1.0 
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 ***********************************************************************/
package com.ge.research.sadl.model.visualizer;

import java.io.IOException;
import java.util.List;

import javax.activation.DataSource;

import com.ge.research.sadl.reasoner.ResultSet;

/**
 * 
 * @author 200005201
 *
 * The type of graph supported by this visualizer interface is a directed graph consisting of nodes and edges.
 * For a discussion of various serialization formats, see
 * 		https://arxiv.org/pdf/1503.02781.pdf
 * 		
 * The following capabilities are needed to support the obvious graphing needs 
 *  of ontologies:
 *  - Nodes with text descriptions
 *  - Directed edges
 *  - Edges labeled
 *   
 * In addition, descriptive information for each node and/or edge label may be included, e.g., 
 *  how to color the edge, how to color or shape the node.
 */
public interface IGraphVisualizer {
	public enum Orientation {TD, LR}
	
	/**
	 * Method to initialize the graph renderer
	 * 
	 * @param tempDir--the fully qualified path to a file system directory into which temporary files may be written
	 * @param baseFileName--a base filename for use in generating temporary and output files associated with the graph to be rendered (may be called repeatedly for multiple graph generation
	 * @param graphName--the name to be given to this graph
	 * @param anchorNode--a node (if any else null) to be colored or otherwise marked as the "anchor" node of the graph
	 * @param orientation--orientation of the graph, top-down (Orientation.TD) or left-right (Orientation.LR)
	 * @param description--description text for this graph (if any else null)
	 */
	public void initialize(String tempDir, String baseFileName, String graphName, String anchorNode, Orientation orientation, String description);

	/**
	 * Call this method to render a graph using data from a specified CSV file
	 *  
	 * @param csvFilepath -- the complete path and name of a CSV file
	 * @return -- a List of errors encountered while rendering the graph else null if no errors
	 * @throws IOException 
	 */
	public List<String> graphCsvData(String csvFilepath) throws IOException;
	
	/**
	 * Call this method to render a graph using data from a specified CSV DataSource 
	 * 	(serialized stream, from a String, from a File, etc.)
	 *  
	 * @param ds -- the DataSource containing the CSV data
	 * @return -- a List of errors encountered while rendering the graph else null if no errors
	 * @throws IOException 
	 */
	public List<String> graphCsvData(DataSource ds) throws IOException;

	/**
	 * Call this method to render a graph using data from a query's returned ResultSet 
	 *  
	 * @param ds -- the DataSource containing the CSV data
	 * @return -- a List of errors encountered while rendering the graph else null if no errors
	 * @throws IOException 
	 */
	public List<String> graphResultSetData(ResultSet rs) throws IOException;
	
}
