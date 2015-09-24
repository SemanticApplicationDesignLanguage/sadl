package com.ge.research.sadl.prolog.ontologyinterface;

import java.io.File;

import com.ge.research.sadl.prolog.fileinterface.FileInterface;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;



public class OntologyInterface{
	private IReasoner jenareasoner;
	private IConfigurationManager configMgr;
	
	public OntologyInterface(){
		
	}
	
	public int initializeOntologyInterface(String KBIdentifier, String modelName, String repoType) throws ConfigurationException, ReasonerNotFoundException, QueryParseException, QueryCancelledException {
		if (configMgr == null) {
			configMgr = new ConfigurationManager(KBIdentifier, repoType);
		}
		setJenareasoner(configMgr.getOtherReasoner("com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin"));
		
		int jenaReasonerStatus = getJenareasoner().initializeReasoner(KBIdentifier, modelName, repoType);
		if (jenaReasonerStatus == 0){
				System.out.println("Jena Reasoner initialization from Prolog returned failure status 0.");
		}
		return jenaReasonerStatus;
	}
	
	public void setConfigMgr(IConfigurationManager cm) {
		configMgr = cm;
	}
	
	public int preparePrologFiles(String KBIdentifier) throws QueryParseException, QueryCancelledException {
		// writing triples to prolog
		System.out.println("Writing triples to prolog");
		ResultSet results = getJenareasoner().ask("select distinct ?x ?y ?z where {?x ?y ?z}");
		Object[][] resultdata = results.getData();
		String outFile = KBIdentifier + File.separator + "rdf.pl";
		FileInterface.writeFile(outFile, "", false);
		for (int row=0; row < resultdata.length; row++){
			String toWrite = "rdf(";
			for (int col=0; col < resultdata[row].length; col++){
				toWrite += "\'" + formatString(resultdata[row][col].toString()) + "\'";
				if (col < resultdata[row].length-1)
					toWrite += ",";
				else
					toWrite += ").\n";
			}
			FileInterface.writeFile(outFile, toWrite, true);
		}
		
		/* write direct instances and subclasses */
		boolean ignore = false;
		results = getJenareasoner().ask("select distinct ?x ?y where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?y}");
		if (results != null){
			resultdata = results.getData();
			for (int row=0; row < resultdata.length; row++){
				ignore = false;
				String toWrite = "holds('_instanceOf_',";
				for (int col=0; col < resultdata[row].length; col++){
					if (ignoreNode(resultdata[row][col].toString())){
						ignore = true;
						break;
					}
					toWrite += "\'" + formatString(resultdata[row][col].toString()) + "\'";
					if (col < resultdata[row].length-1)
						toWrite += ",";
					else
						toWrite += ").\n";
				}
				if (!ignore)
					FileInterface.writeFile(outFile, toWrite, true);
			}
		}
		
		results = getJenareasoner().ask("select distinct ?x ?y where {?x <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?y}");
		if (results != null){
			resultdata = results.getData();
			for (int row=0; row < resultdata.length; row++){
				ignore = false;
				String toWrite = "holds('_subClassOf_',";
				for (int col=0; col < resultdata[row].length; col++){
					if (ignoreNode(resultdata[row][col].toString())){
						ignore = true;
						break;
					}
					toWrite += "\'" + formatString(resultdata[row][col].toString()) + "\'";
					if (col < resultdata[row].length-1)
						toWrite += ",";
					else
						toWrite += ").\n";
				}
				if (!ignore)
					FileInterface.writeFile(outFile, toWrite, true);
			}
		}
		/*
		results = jenareasoner.ask("select distinct ?x ?y where {?x <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?z. ?z <http://www.w3.org/2002/07/owl#intersectionOf> ?l. ?l <http://jena.hpl.hp.com/ARQ/list#member> ?y}");
		if (results != null){
			resultdata = results.getData();
			for (int row=0; row < resultdata.length; row++){
				ignore = false;
				String toWrite = "holds('_dsubClassOf_',";
				for (int col=0; col < resultdata[row].length; col++){
					if (ignoreNode(resultdata[row][col].toString())){
						ignore = true;
						break;
					}
					toWrite += "\'" + formatString(resultdata[row][col].toString()) + "\'";
					if (col < resultdata[row].length-1)
						toWrite += ",";
					else
						toWrite += ").\n";
				}
				if (!ignore)
					FileInterface.writeFile(outFile, toWrite, true);
			}
		}*/
		
		return 1;
	}
	
	private boolean ignoreNode(String node){
		if (node.contains("(blank node)"))
			return true;
		if (node.contains("http://www.w3.org/2002/07/owl#"))
			return true;
		if (node.contains("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
			return true;
		if (node.contains("http://www.w3.org/2000/01/rdf-schema#subClassOf"))
			return true;
		return false;
	}
	
	private String formatString (String input){
		return input.replace("\"", "").replace("\'", "");
	}
	
	public ResultSet queryAndWrite(String query, String filename, String predname) throws QueryParseException, QueryCancelledException {
		System.out.println("Running query: " + query);
		ResultSet results = getJenareasoner().ask("select ?x where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class>}");
		return results;
		
		//return 0;
	}
	
	public ResultSet runNonPrologQuery(String query) throws QueryParseException, QueryCancelledException {
		System.out.println("Running query: " + query);
		System.out.println("Invoking Jena Reasoner");
		ResultSet results = getJenareasoner().ask(query);
		return results;
		
		//return 0;
	}

	public IReasoner getJenareasoner() {
		return jenareasoner;
	}

	private void setJenareasoner(IReasoner jenareasoner) {
		this.jenareasoner = jenareasoner;
	}
	
//	public static void dummy1()
//	{
//		
//	}
//	
//	public void dummy()
//	{
//		
//	}
	
}
