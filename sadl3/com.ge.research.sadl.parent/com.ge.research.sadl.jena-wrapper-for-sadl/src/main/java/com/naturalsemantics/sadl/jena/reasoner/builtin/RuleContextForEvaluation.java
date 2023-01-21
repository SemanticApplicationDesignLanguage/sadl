/************************************************************************
 * Copyright © 2023 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemantics.sadl.jena.reasoner.builtin;

import java.util.Iterator;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Node_Literal;
import org.apache.jena.graph.Node_URI;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.rdf.model.impl.StmtIteratorImpl;
import org.apache.jena.reasoner.InfGraph;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.Rule;
import org.apache.jena.reasoner.rulesys.impl.SafeTripleIterator;
import org.apache.jena.util.iterator.ClosableIterator;

public class RuleContextForEvaluation implements org.apache.jena.reasoner.rulesys.RuleContext {

	private BindingEnvironmentForEvaluation bindingEnv = null;
	private OntModel theModel = null;
	
	public RuleContextForEvaluation(BindingEnvironmentForEvaluation befe, OntModel model) {
		bindingEnv = befe;
		theModel = model;
	}
	
	@Override
	public BindingEnvironment getEnv() {
		return bindingEnv;
	}

	@Override
	public InfGraph getGraph() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Rule getRule() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setRule(Rule rule) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean contains(Triple t) {
		return contains(t.getSubject(), t.getPredicate(), t.getObject());
	}

	@Override
	public boolean contains(Node s, Node p, Node o) {
		if (s instanceof Node_URI && p instanceof Node_URI) {
			RDFNode onode = null;
			if (o instanceof Node_URI) {
				onode = theModel.getResource(((Node_URI)o).getURI());
			}
			else if (o instanceof Node_Literal) {
				onode = theModel.createTypedLiteral( ((Node_Literal)o).getLiteral());
			}
			return theModel.contains(theModel.getResource(((Node_URI)s).getURI()), 
					theModel.getProperty(((Node_URI)p).getURI()), onode);
		}
		return false;
	}

	@Override
	public ClosableIterator<Triple> find(Node s, Node p, Node o) {
		if (s instanceof Node_URI && p instanceof Node_URI) {
			RDFNode onode = null;
			if (o instanceof Node_URI) {
				onode = theModel.getResource(((Node_URI)o).getURI());
			}
			else if (o instanceof Node_Literal) {
				onode = theModel.createTypedLiteral( ((Node_Literal)o).getLiteral());
			}
			StmtIterator itr = theModel.listStatements(theModel.getResource(((Node_URI)s).getURI()), 
					theModel.getProperty(((Node_URI)p).getURI()), onode);
			if (itr.hasNext()) {
				return createCloseableIterator(itr);
			}
		}
		return null;
	}

	 private <T> ClosableIterator<Triple> createCloseableIterator(Iterator<Statement> iterator) {
		    return new ClosableIterator<Triple>() {
		      @Override public void close() {
//		    	  iterator.close();
		      }

		      @Override public boolean hasNext() {
		        return iterator.hasNext();
		      }

		      @Override public Triple next() {
		        Statement stmt = iterator.next();
		        Triple triple = new Triple(stmt.getSubject().asNode(), stmt.getPredicate().asNode(), stmt.getObject().asNode());
				return triple;
		      }
		    };
		  }
	 
	@Override
	public void silentAdd(Triple t) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void add(Triple t) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void remove(Triple t) {
		// TODO Auto-generated method stub
		
	}

}
