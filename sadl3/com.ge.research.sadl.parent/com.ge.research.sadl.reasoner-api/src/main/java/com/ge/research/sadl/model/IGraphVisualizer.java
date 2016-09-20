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
package com.ge.research.sadl.model;

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
 * To support the obvious graphing needs of ontologies, one must have the following capabilities:
 * 	- Directed
 *  - Edges labeled
 *  - Vertices of differing types identified by shape and/or color
 *   
 * In addition, it would be nice to allow descriptive information for each node and edge label that might be used for, 
 *  for example, mouse over display.
 */
public interface IGraphVisualizer {

	public boolean graphFromCsvFile(String csvFilepath);
	
	public boolean graphFromResultSet(ResultSet rs);
	
	public boolean graphFromCsvDataSource(DataSource ds);
	
}
