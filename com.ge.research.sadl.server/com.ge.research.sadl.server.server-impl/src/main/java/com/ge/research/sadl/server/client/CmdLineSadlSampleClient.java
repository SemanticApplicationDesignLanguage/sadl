/************************************************************************
 * Copyright (c) 2007-2015 - General Electric Company, All Rights Reserved
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

import java.io.IOException;
import java.io.InputStreamReader;

import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.server.ISadlServer;
import com.ge.research.sadl.server.server.SadlServerImpl;

/**
 * Calls local operations on an instance of SadlServerImpl.
 * The message shown in the console is the result from the service
 */
public class CmdLineSadlSampleClient {
    private static Logger log = LoggerFactory.getLogger("CmdLineSadlSampleClient");

    /**
     * Program will create an axis client with the given context,
     * will get an instance of the <code>SadlService</code>
     * and then invoke its remote methods.
     *
     * Program exits cleanly upon successful completion.
     *
     * @param args url to the SadlService
     *
     * @throws Exception Any communication or configuration error will
     *         throw back a stack trace.
     */
    public static void main(String[] args) throws Exception {
    	ISadlServer ssc =  new SadlServerImpl();

        log.info("Invoking remote 'getClassName' operation");
        try {
        	String retVal = (String) ssc.getClassName();
        	log.info("Response from 'getClassName' operation: "+retVal);
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
        
        log.info("Invoking remote 'getServiceVersion' operation");
        try {
        	String retVal = (String) ssc.getServiceVersion();
        	log.info("Response from 'getServiceVersion' operation: "+retVal);
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
        
        log.info("Invoking remote 'collectTimingInformation' operation");
        try {
        	ssc.collectTimingInformation(true);
        	log.info("Response from 'collectTimingInformation' operation.");
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
        
//        log.info("Invoking remote 'setOwlFileOutputFormat' operation");
//        try {
//        	ssc.setOwlFileOutputFormat("xml");
//        	log.info("Response from 'setOwlFileOutputFormat' operation.");
//        } catch (Exception e) {
//        	log.error("Caught Exception");
//        	log.error("Fault message: "+e.getMessage());
//        }

        String kbroot = getConsoleInput("Kbase root (KB_ROOT): "); // InScope-CFM56-7";
        log.info("Invoking remote 'selectServiceModel' operation");
        try {
        	ssc.setKbaseRoot(kbroot);
        	log.info("KBase root set to: "+kbroot);
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }

        String serviceName = getConsoleInput("Service Name: "); // InScope-CFM56-7";
        log.info("Invoking remote 'selectServiceModel' operation");
        try {
        	String dummy = ssc.selectServiceModel(serviceName);
        	log.info("Response from 'selectServiceModel' operation: "+dummy);
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
        
        String fileName = getConsoleInput("Input data file name or return if no input file: "); //"G:/eclipse302/eclipse/workspace/SadlServerV2Standalone/DataModels/InScope/cfm56-7_full_test1.owl";
        if (fileName != null && fileName.length() > 0) {
	        log.info("Invoking remote 'sendData' operation");
	        String inputFormat = "RDF/XML";
	        try {
	    		DataSource dataSrc = new FileDataSource(fileName);
	        	Boolean sdRc = ssc.sendData(dataSrc, inputFormat);
	        	log.info("Response from 'sendData' operation: "+sdRc.toString());
	        } catch (Exception e) {
	        	log.error("Caught Exception");
	        	log.error("Fault message: "+e.getMessage());
	        }
        }
        
        String query1 = getConsoleInput("SPARQL query: "); // "select ?cw  where {<Transfer_Gearbox_Module-111111> <candidate_Workscope> ?cw }";
        log.info("Invoking remote 'prepareQuery' operation");
        String query1p = "";
        try {
        	query1p = ssc.prepareQuery(query1);
        	System.out.println("Response from 'prepareQuery' operation: "+query1p);
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
        
        log.info("Invoking remote 'query' operation");
        try {
    		ResultSet results = ssc.query(query1p);
    		if (results != null) {
    			System.out.println("Response from 'query' operation:\n" + results.toString());
        	} else {
        		System.out.println("Response from 'query' operation: No result ");
        	}
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
   
        log.info("Invoking remote 'getTimingInformation' operation");
        try {
        	ReasonerTiming[] tiList = ssc.getTimingInformation();
        	System.out.println(    "Response from 'getTimingInformation' operation: ");
        	for (int i=0; i<tiList.length; i++) {
        		System.out.println("   >>> "+tiList[i].toString());
    		}
        } catch (Exception e) {
        	log.error("Caught Exception");
        	log.error("Fault message: "+e.getMessage());
        }
        
        getConsoleInput("Run complete; press any key to end"); // "select ?cw  where {<Transfer_Gearbox_Module-111111> <candidate_Workscope> ?cw }";
    }
    
    private static String getConsoleInput(String prompt) {
    	System.out.println(prompt);
    	StringBuffer newq = new StringBuffer();
    	InputStreamReader sr = new InputStreamReader(System.in);

    	char[] onechar = new char[1];

    	while (true) {
    		try {
    			sr.read(onechar, 0, 1);
    		}
    		catch (IOException e) {
    			break;
    		}

    		if (onechar[0] == '\n' || onechar[0] == '\r') {
    			break;
    		}
    		else {
    			newq.append(onechar);
    		}
    	}
    	log.info("Value received for '" + prompt + "': " + newq.toString());

    	return newq.toString();
    }
}
