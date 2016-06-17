package com.ge.research.sadl.jena;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.HashMap;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFWriter;

public class SortedOwlNtriples {

	public static final String RDF_XML_FORMAT = "RDF/XML"; // default
	public static final String RDF_XML_ABBREV_FORMAT = "RDF/XML-ABBREV";
	public static final String N_TRIPLE_FORMAT = "N-TRIPLE";
	public static final String N3_FORMAT = "N3";
	public static void main(String[] args) {
		if (args.length < 1) {
			System.err.println("Input OWL file name must be first command-line argument");
			return;
		}
		File in = new File(args[0]);
		if (!in.exists()) {
			try {
				System.err.println("Input OWL file '" + in.getCanonicalPath() + "' does not exist");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		String format = RDF_XML_ABBREV_FORMAT;
		if (args[0].trim().endsWith(".nt")) {
			format = N_TRIPLE_FORMAT;
		}
		else if (args[0].endsWith(".n3")) {
			format = N3_FORMAT;
		}
		OntModel om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		om.getDocumentManager().setProcessImports(false);
		try {
			InputStream is = new FileInputStream(in);
			om.read(is, format);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		RDFWriter w = om.getWriter(N_TRIPLE_FORMAT);
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		w.write(om, out, format);
		Charset charset = Charset.forName("UTF-8"); 
		CharSequence seq = new String(out.toByteArray(), charset);
		HashMap<String,HashMap<String, String>> map = new HashMap<String,HashMap<String, String>>();
		String[] arr = seq.toString().split("\n");
		for (int i = 0; i < arr.length; i++) {
			String ln = arr[i];
			int start = ln.indexOf("<");
			int end = ln.indexOf(">", start);
			String key = ln.substring(start, end);
			String rest = ln.substring(end + 1);
//			System.out.println("key=" + key + ": " + ln);
			if (map.containsKey(key)) {
				HashMap<String,String> prior = map.get(key);
				prior.put(rest, ln);
			}
			else {
				HashMap<String,String> lines = new HashMap<String,String>();
				lines.put(rest,ln);
				map.put(key, lines);
			}
		}
		try {
			File sortedFile = new File(in.getParentFile().getCanonicalPath() + File.separator + in.getName() + ".sorted.nt");
			PrintWriter writer = new PrintWriter(new FileWriter(sortedFile));
			Object[] keys = map.keySet().toArray();
			Arrays.sort(keys);
			for (int i = 0; i < keys.length; i++) {
				String key = keys[i].toString();
				HashMap<String,String> lines = map.get(key);
				Object[] linearray = lines.keySet().toArray();
				Arrays.sort(linearray);
				for (int j = 0; j < linearray.length; j++) {
					writer.println(lines.get(linearray[j]));
				}
			}
			writer.close();
			System.out.println("Sorted n-triples OWL file written to '" + sortedFile.getCanonicalPath() + "'");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
