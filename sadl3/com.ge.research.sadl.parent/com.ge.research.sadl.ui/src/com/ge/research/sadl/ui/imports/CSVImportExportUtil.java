/**********************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved.   This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * $Id: CSVImportExportUtil.java,v 1.1 2014/01/22 20:32:49 crapo Exp $
 * 
 * Contributors: 
 * IBM - Initial API and implementation
 *     Barry Hathaway - modified for CVS imports into SADL
 *******************************************************************************/
package com.ge.research.sadl.ui.imports;

/**
 * @author psun
 *
 */
public class CSVImportExportUtil 
{
	public static final String ASCII = "US-ASCII"; //$NON-NLS-1$
	public static final String ISONLATIN = "ISO-8859-1"; //$NON-NLS-1$
	public static final String UTF8 = "UTF-8"; //$NON-NLS-1$
	public static final String UTF16 = "UTF-16"; //$NON-NLS-1$
	public static final String UTF16LE = "UTF-16LE"; //$NON-NLS-1$
	public static final String UTF16BE = "UTF-16BE"; //$NON-NLS-1$
		
	private static final String SEPERATOR = ","; //$NON-NLS-1$
	private static final String LINEFEED = "\n"; //$NON-NLS-1$
	private static final String CARRIAGE_RETURN = "\r"; //$NON-NLS-1$
	private static final String COLON = ":"; //$NON-NLS-1$

	//private static int ecCounter = 1;	
	private static final CSVImportExportUtil instance = new CSVImportExportUtil();

	private static long lineCounter = 1;
	
	public static CSVImportExportUtil getInstance()
	{
		return instance;
	}
	
	protected CSVImportExportUtil()
	{
	}
		
	/*
	public void importCSV(IDatapool datapool, 
							String csvFileName, 
							boolean isFirstRowVariableNameType, 
							boolean isFirstColEqClsName,
							String importEncoding)
				throws IOException, CorruptCSVFileException
	{
		if(datapool == null || csvFileName == null)
			return;
		try
		{			
			InputStreamReader inputStreamReader = null;
			if(importEncoding.length() == 0)
				inputStreamReader = new InputStreamReader(new FileInputStream(csvFileName));  //$NON-NLS-1$
			else
				inputStreamReader = new InputStreamReader(new FileInputStream(csvFileName), importEncoding);  //$NON-NLS-1$
				
			CSVBufferedReader bufferedReader = new CSVBufferedReader(inputStreamReader);
			String firstLine = bufferedReader.readLine();
			
			if(isFirstRowVariableNameType)
			{
				createVariablesFromFile(datapool, firstLine, isFirstColEqClsName);
				firstLine = bufferedReader.readLine();
				lineCounter++;
			}
			else
				createVariables(datapool, firstLine, isFirstColEqClsName);
			
			for(String line = firstLine; 
				line != null; 
				line = bufferedReader.readLine(), lineCounter++)
			{
				createRecord(datapool, line, isFirstColEqClsName);
			}
			bufferedReader.close();
		}
		catch(IOException e)
		{
			throw e;
		}
		catch(CorruptCSVFileException e)
		{
			throw e;
		}
		finally
		{
			lineCounter = 1;
		}
	}
	*/
	
	/*
	public void exportCSV(IDatapool datapool, 
							String csvFileName, 
							boolean includeVariables, 
							boolean includeEquivalenceClassNames,
							boolean includeTags,
							String exportEncoding)
	{
		if(datapool == null || csvFileName == null)
			return;
		try
		{
			OutputStreamWriter outputStreamWriter = null;
			if(exportEncoding.length() == 0)
				outputStreamWriter = new OutputStreamWriter(new FileOutputStream(csvFileName));  //$NON-NLS-1$
			else
				outputStreamWriter = new OutputStreamWriter(new FileOutputStream(csvFileName), exportEncoding);  //$NON-NLS-1$
			BufferedWriter bufferedWriter = new BufferedWriter(outputStreamWriter);
			if(includeVariables)
				writeVariables(datapool, bufferedWriter, true, includeEquivalenceClassNames);
			writeRecords(datapool, bufferedWriter, includeEquivalenceClassNames, includeTags);
			bufferedWriter.close();
		}
		catch(IOException e)
		{
		}
		finally
		{
			lineCounter = 1;
		}		
	}
	*/
	
	/*
	private void createVariablesFromFile(IDatapool datapool, String variableString, boolean ignoreFirst) throws CorruptCSVFileException
	{
		boolean first = true;
		CSVTokenizer tokenizer = new CSVTokenizer(variableString);
		while(tokenizer.hasMoreTokens())
		{
			String nameTypePair = tokenizer.nextToken();
			if(ignoreFirst && first)
			{
				first = false;
				continue;
			}
			int separatorIndex = nameTypePair.indexOf(DatapoolPlugin.getResourceString("DATA_EDT_DIVIDER")); //$NON-NLS-1$
			String name = new String();
			String type = new String();
			if(separatorIndex == -1 )
			{
				name = nameTypePair;
			}
			else
			{
				name = nameTypePair.substring(0, separatorIndex);
				type = nameTypePair.substring(separatorIndex + 2);
			}
			
			String nameErrorFormat = DatapoolPlugin.getResourceString("DATA_COL_DLG_ERROR_NAME_FORMAT");
			if(!DatapoolUtil.getInstance().isVariableNameUnique(datapool, name, null))
			{
				Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_CSV_LINE"),  //$NON-NLS-1$
											NumberFormat.getInstance().format(lineCounter), 
											COLON, 
											DatapoolPlugin.getResourceString("DATA_COL_DLG_ERROR_NAME_NOT_UNIQUE")}; //$NON-NLS-1$
				String error = MessageFormat.format(nameErrorFormat, messageElements); //$NON-NLS-1$
				throw new CorruptCSVFileException(error);				
			}
			if(!DatapoolUtil.getInstance().isVariableNameValid(name))
			{
				Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_CSV_LINE"),  //$NON-NLS-1$
											NumberFormat.getInstance().format(lineCounter), 
											COLON, 
											DatapoolPlugin.getResourceString("DATA_COL_DLG_ERROR_NAME_NOT_VALID")}; //$NON-NLS-1$
				String error = MessageFormat.format(nameErrorFormat, messageElements); //$NON-NLS-1$
				throw new CorruptCSVFileException(error);				
				
			}
			if(!DatapoolUtil.getInstance().isVariableTypeValid(type))
			{
				Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_CSV_LINE"),  //$NON-NLS-1$
											NumberFormat.getInstance().format(lineCounter), 
											COLON, 
											DatapoolPlugin.getResourceString("DATA_COL_DLG_ERROR_TYPE_NOT_VALID")}; //$NON-NLS-1$
				String error = MessageFormat.format(nameErrorFormat, messageElements); //$NON-NLS-1$
				throw new CorruptCSVFileException(error);				
			}
			IDatapoolVariable variable = datapool.constructVariable();
			variable.setName(name);
			IDatapoolSuggestedType suggestedType = (IDatapoolSuggestedType)variable.getSuggestedType();
			TypeChecker.getInstance().setVariableType(suggestedType, type);
			variable.setSuggestedType(suggestedType);
			datapool.appendVariable(variable);
		}
	}
	*/

	/*
	private void createVariables(IDatapool datapool, String varaibleString, boolean ignoreFirst)
	{
		CSVTokenizer tokenizer = new CSVTokenizer(varaibleString);
		int counter = 1;
		if(ignoreFirst && tokenizer.hasMoreTokens())
			tokenizer.nextToken();
		while(tokenizer.hasMoreTokens())
		{
			tokenizer.nextToken();
			Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_VARIABLE_NAME"), String.valueOf(counter)}; //$NON-NLS-1$
			String name = MessageFormat.format(DatapoolPlugin.getResourceString("DATA_VARIABLE_NAME_FORMAT"), messageElements); //$NON-NLS-1$
			IDatapoolVariable variable = datapool.constructVariable();
			IDatapoolSuggestedType suggestedType = (IDatapoolSuggestedType)variable.getSuggestedType();
			suggestedType.setSuggestedClassName(new String());
			variable.setSuggestedType(suggestedType);
			variable.setName(name);
			datapool.appendVariable(variable);
			counter++;
		}
	}
	*/
	
	/*
	private void createRecord(IDatapool datapool, String recordString, boolean useSpecifiedEC) throws CorruptCSVFileException
	{
		CSVTokenizer tokenizer = new CSVTokenizer(recordString);
		IDatapoolEquivalenceClass equivalenceClass = null;
		String recordIndexStr = null;
		int recordIndex = -1;
		String nameErrorFormat = DatapoolPlugin.getResourceString("DATA_COL_DLG_ERROR_NAME_FORMAT");
		if(useSpecifiedEC)
		{
			String ecName = tokenizer.nextToken();
			int separatorIndex = ecName.indexOf(DatapoolPlugin.getResourceString("DATA_EDT_DIVIDER")); //$NON-NLS-1$
			
			if(separatorIndex >= 0)
			{
				try
				{
					recordIndexStr = ecName.substring(separatorIndex + 2);
					Number number = NumberFormat.getInstance().parse(recordIndexStr);
					recordIndex = number.intValue();
				}
				catch(Exception e)
				{
				}
				ecName = ecName.substring(0, separatorIndex);
			}
			
			if(!DatapoolUtil.getInstance().isEquivalenceClassNameValid(ecName))
			{
				Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_CSV_LINE"),  //$NON-NLS-1$
											NumberFormat.getInstance().format(lineCounter), 
											COLON, 
											DatapoolPlugin.getResourceString("DATA_ROW_GRP_DLG_ERROR_NAME_NOT_VALID")}; //$NON-NLS-1$
				String error = MessageFormat.format(nameErrorFormat, messageElements); //$NON-NLS-1$
				throw new CorruptCSVFileException(error);								
			}
			int ecIndex = datapool.getEquivalenceClassIndex(ecName);
			if(ecIndex >= 0)
				equivalenceClass = (IDatapoolEquivalenceClass)datapool.getEquivalenceClass(ecIndex);
			else
			{
				equivalenceClass = datapool.constructEquivalenceClass();
				equivalenceClass.setName(ecName);
				datapool.appendEquivalenceClass(equivalenceClass);
			}		
		}
		else
		{
			// ecCounter++;
			// equivalenceClass = datapool.constructEquivalenceClass();
			// datapool.appendEquivalenceClass(equivalenceClass);
			
			// by default, import all records into the default equivalence class.
			if(datapool.getEquivalenceClassCount() == 0)
			{
				equivalenceClass = datapool.constructEquivalenceClass();
				datapool.appendEquivalenceClass(equivalenceClass);
			}
			else
			{
				int defaultIndex = datapool.getDefaultEquivalenceClassIndex();
				defaultIndex = defaultIndex < 0 ? 0 : defaultIndex;
				equivalenceClass = (IDatapoolEquivalenceClass)datapool.getEquivalenceClass(defaultIndex);
			}
		}
		Vector cellValues = new Vector();
		while(tokenizer.hasMoreTokens())
		{
			String cellValue = tokenizer.nextToken();
			cellValues.add(cellValue);
		}
		
		String[] recordValues = new String[cellValues.size()];
		for(int i = 0; i < cellValues.size(); i++)
		{
			recordValues[i] = (String)cellValues.get(i);
		}
		IDatapoolRecord record = equivalenceClass.constructRecord(recordValues);
		if(recordIndex != -1)
		{
			IDatapoolRecord existingRecord = null;
			try
			{
				existingRecord = (IDatapoolRecord)equivalenceClass.getRecord(recordIndex);
			}
			catch(Exception e)
			{
			}
			if(existingRecord != null)
			{
				Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_CSV_LINE"),  //$NON-NLS-1$
											NumberFormat.getInstance().format(lineCounter), 
											COLON, 
											DatapoolPlugin.getResourceString("DATA_CSV_ERROR_RECORD_INDEX_DUP")}; //$NON-NLS-1$
				String error = MessageFormat.format(nameErrorFormat, messageElements); //$NON-NLS-1$
				throw new CorruptCSVFileException(error);												
			}
			if(recordIndex > equivalenceClass.getRecordCount())
			{
				Object[] messageElements = {DatapoolPlugin.getResourceString("DATA_CSV_LINE"),  //$NON-NLS-1$
											NumberFormat.getInstance().format(lineCounter), 
											COLON, 
											DatapoolPlugin.getResourceString("DATA_CSV_ERROR_RECORD_INDEX_ORDER")}; //$NON-NLS-1$
				String error = MessageFormat.format(nameErrorFormat, messageElements); //$NON-NLS-1$
				throw new CorruptCSVFileException(error);																
			}
			else
				equivalenceClass.insertRecord(record, recordIndex);
		}
		else
			equivalenceClass.appendRecord(record);
	}
	*/
	
	/*
	private void writeVariables(IDatapool datapool, BufferedWriter writer, boolean includeTypes, boolean includeEquivalenceClasses)
	{
		String output = new String();
		int count = datapool.getVariableCount();
		if(count != 0)
		{			
			if(includeEquivalenceClasses)
			{
				Object[] messageElements = {output, SEPERATOR};
				output = MessageFormat.format("{0}{1}", messageElements); //$NON-NLS-1$
			}
			for(int i = 0; i < count; i++)
			{
				IDatapoolVariable variable = (IDatapoolVariable)datapool.getVariable(i);
				String name = variable.getName();
				String separator = new String();
				if(i != count - 1)
					separator = SEPERATOR;
				if(includeTypes)
				{
					String type = variable.getSuggestedType().getSuggestedClassName();
					Object[] preprocessedElements = {name, DatapoolPlugin.getResourceString("DATA_EDT_DIVIDER"), type}; //$NON-NLS-1$
					String preprocessedOutput = MessageFormat.format("{0}{1}{2}", preprocessedElements); //$NON-NLS-1$
					preprocessedOutput = processString(preprocessedOutput);
					Object[] messageElements = {output, preprocessedOutput, separator};
					output = MessageFormat.format("{0}{1}{2}", messageElements); //$NON-NLS-1$
				}
				else
				{
					name = processString(name);
					Object[] messageElements = {output, name, separator};
					output = MessageFormat.format("{0}{1}{2}", messageElements); //$NON-NLS-1$
				}	
			}
			Object[] messageElements = {output, LINEFEED};
			output = MessageFormat.format("{0}{1}", messageElements); //$NON-NLS-1$
		}
		else
		{
			output = LINEFEED;
		}
		try
		{
			writer.write(output);
		}
		catch(IOException e)
		{
		}
	}
	*/
	
	/*
	private void writeRecords(IDatapool datapool, BufferedWriter writer, boolean includeEquivalenceClasses, boolean includeTags)
	{
		StringBuffer output = new StringBuffer();
		for(int i = 0; i < datapool.getEquivalenceClassCount(); i++)
		{
			IDatapoolEquivalenceClass equivalenceClass = (IDatapoolEquivalenceClass)datapool.getEquivalenceClass(i);
			String equivalenceClassName = new String();
			if(includeEquivalenceClasses)
			{
				equivalenceClassName = equivalenceClass.getName();
				Object[] messageElements = {equivalenceClassName, DatapoolPlugin.getResourceString("DATA_EDT_DIVIDER")}; //$NON-NLS-1$
				equivalenceClassName = MessageFormat.format("{0}{1}", messageElements); //$NON-NLS-1$
			}
			for(int j = 0; j < equivalenceClass.getRecordCount(); j++)
			{
				IDatapoolRecord record = (IDatapoolRecord)equivalenceClass.getRecord(j);
				if(includeEquivalenceClasses)
				{
					Object[] preprocessedElements = {equivalenceClassName, NumberFormat.getInstance().format(j)};
					String preprocessedOutput = MessageFormat.format(DatapoolPlugin.getResourceString("DATA_VARIABLE_NAME_FORMAT"), preprocessedElements); //$NON-NLS-1$
					preprocessedOutput = processString(preprocessedOutput);
					output.append(preprocessedOutput).append(SEPERATOR);
				}
				int count = record.getCellCount();
				for(int k = 0; k < count; k++)
				{
					IDatapoolCell cell = (IDatapoolCell)record.getCell(k);
					String cellValue = new String();
					if(includeTags)
						cellValue = cell.getPersistedRepresentation();
					else
						cellValue = cell.getStringValue();
						
					output.append(processString(cellValue));
					if(k != count - 1)
						output.append(SEPERATOR);
				}
				
				output.append(CSVBufferedReader.LINE_SEPARATOR);
			}
		}
		try
		{
			writer.write(output.toString());
		}
		catch(IOException e)
		{
		}				
	}
	*/
	
	/*
	private String processString(String value)
	{
		if(value == null)
			return null;
		
		// https://bugs.eclipse.org/bugs/show_bug.cgi?id=117932
		// performance optimization
		if(value.indexOf(CSVTokenizer.DOUBLEQUOTE) > -1)
		{
			value = value.replaceAll(CSVTokenizer.DOUBLEQUOTE, CSVTokenizer.TWO_DOUBLEQUOTES);
			return (CSVTokenizer.DOUBLEQUOTE + value + CSVTokenizer.DOUBLEQUOTE);
		}
		
		if(value.indexOf(CSVTokenizer.COMMA) > -1 ||
			value.indexOf(LINEFEED) > -1 ||
			value.indexOf(CARRIAGE_RETURN) > -1 ||
			value.trim().length() < value.length())
			return (CSVTokenizer.DOUBLEQUOTE + value + CSVTokenizer.DOUBLEQUOTE);
	
		return value;
	}
	*/
	
}
