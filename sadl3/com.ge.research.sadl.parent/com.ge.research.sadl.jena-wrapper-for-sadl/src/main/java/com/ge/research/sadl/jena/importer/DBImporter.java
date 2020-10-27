package com.ge.research.sadl.jena.importer;

import java.io.IOException;

import javax.activation.DataSource;

public class DBImporter extends TabularDataImporter {
	private static final String DB_IMPORTER = "DBImporter";

	public DBImporter() {
		super();
		importerType = DB_IMPORTER;
	}
	
	/**
	 * Method to set the DB import source
	 * 
	 * @param -- TBD	// Sanand, add arguments, as necessary
	 */
	public void setImportDatabaseSource() {
		// I think a JDBC ResultSet can be returned as an InputStream
		// see https://stackoverflow.com/questions/11209818/java-sql-result-to-inputstream
		dataInputStream = null;	// this needs to assign a valid dataInputStream to be used to get lines of data
	}

	@Override
	public void setImportFilename(String _importFilename, boolean _includesHeader) throws IOException {
		throw new IOException("DBImporter cannot use an import filename as input.");
	}

	@Override
	public void setImportDataSource(DataSource importDs, boolean _includesHeader) throws IOException {
		throw new IOException("DBImporter cannot use a DataSource as input.");
	}
	
	@Override
	protected String[] getNextLine() throws IOException {
		// this method will use the dataInputStream to get the next line from the ResultSet
		return null;
	}

	@Override
	protected void cleanup() throws IOException {
		// this method will do any cleanup--closing connections, etc.
	}

}
