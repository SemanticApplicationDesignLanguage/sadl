/************************************************************************
 * Copyright � 2007-2011 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.jena.importer;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.activation.DataSource;

import com.ge.research.sadl.reasoner.IConfigurationManager;

import au.com.bytecode.opencsv.CSVReader;


/**
 * This class converts a CSV file to an OWL model.
 * 
 * @author 200005201
 *
 */
public class CsvImporter extends TabularDataImporter {
	private static final String CSV_IMPORTER = "CsvImporter";
	private CSVReader csvReader;
	private InputStreamReader isReader;


	/**
	 * Class has a null argument constructor.
	 */
	public CsvImporter() {
		super();
		importerType = CSV_IMPORTER;
	}

	public CsvImporter(IConfigurationManager configurationMgr) {
		super(configurationMgr);
		importerType = CSV_IMPORTER;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setCsvFilename(java.lang.String, boolean)
	 */
	@Override
	public void setImportFilename(String _csvFilename, boolean _includesHeader) throws IOException {
		_csvFilename = validateFile(_csvFilename, false);
		dataInputStream = new FileInputStream(_csvFilename);
		isReader = new InputStreamReader(dataInputStream);
		setIncludesHeader(_includesHeader);
		processed = false; // set or reset
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setCsvDataSource(javax.activation.DataSource, boolean)
	 */
	@Override
	public void setImportDataSource(DataSource csvDs, boolean _includesHeader) throws IOException {
		dataInputStream = csvDs.getInputStream();
		isReader = new InputStreamReader(dataInputStream);
		setIncludesHeader(_includesHeader);
		processed = false; // set or reset
	}

	@Override
	protected void cleanup() throws IOException {
		if (getCsvReader() != null) {
			getCsvReader().close();
		}
	}

	@Override
	protected String[] getNextLine() throws IOException {
		return getCsvReader().readNext();
	}

	private CSVReader getCsvReader() {
		if (csvReader == null) {
			csvReader = new CSVReader(isReader);
		}
		return csvReader;
	}

	private void setCsvReader(CSVReader csvReader) {
		this.csvReader = csvReader;
	}
}
