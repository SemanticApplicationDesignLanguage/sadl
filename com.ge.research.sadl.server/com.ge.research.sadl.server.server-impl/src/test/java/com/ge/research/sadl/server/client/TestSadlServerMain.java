/************************************************************************
 * Copyright (c) 2007-2014 - General Electric Company, All Rights Reserved
 *
 * Project: SADL Knowledge Server
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language. 
 * 
 * The SADL Knowledge Server is a set of Java classes implementing 
 * a service interface for deploying ontology-based knowledge bases
 * for use in a client-server environment.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/09 14:06:51 $
 ***********************************************************************/

package com.ge.research.sadl.server.client;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;

import javax.naming.NameNotFoundException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.server.NamedServiceNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;

public class TestSadlServerMain { 
	private String kbaseroot = null;
	
	@Before
	public void setUp() throws Exception {
		URL dataModelsFolder = ClassLoader.getSystemResource("DataModels");
		File kbrootfile = new File(dataModelsFolder.getFile());
		URI kbrootfileuri = kbrootfile.toURI();
		System.out.println("kbroot: " + kbrootfileuri.toString());
		kbaseroot = kbrootfileuri.getRawPath();
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws NameNotFoundException, IOException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, NamedServiceNotFoundException, QueryCancelledException {
		SadlServerMain ssm = new SadlServerMain();
		String args[] = new String[10];
		args[0] = "com.ge.research.sadl.server.server.SadlServerMDImpl";
		args[1] = kbaseroot + "/Advise2";	// kbase folder
		args[2] = "http://www.illinois.edu/advise/SmallExample";		// tbox
		args[3] = null; // args[1] + "";		// abox
		args[4] = "select ?s ?p ?v where {?s ?p ?v}";
		args[5] = "c:\\tmp\\SadlServerTest1.txt";
		args[6] = null;
		args[7] = "DEBUG";
		args[8] = "2";
		args[9] = "c:\\tmp\\SadlServerTest1.Derivations.log";

		String skey = ssm.process(args);
		
		String[] ioc = ssm.getInstancesOfClass("http://www.illinois.edu/advise/Firewall#Firewall"); 
		System.out.println("Instances of :");
		for (int i =0; i < ioc.length; i++) {
			System.out.println("   " + ioc[i]);
		}
	}
		
}
