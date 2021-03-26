/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.3 $ Last modified on   $Date: 2014/12/17 17:26:33 $
 ***********************************************************************/

package com.ge.research.sadl.model;

import java.util.Objects;

/**
 * This class encapsulates the important information describing an error in a call to ModelManager to create 
 * model content. Various constructors allow the relevant information to be passed to populate a class instance.
 * Information can include:
 * 
 *     int argIdx -- the argument of the method call which caused the error (0-based; first argument is 0)
 *  ExistingNamePart extNameIdx -- the portion of an existing name that caused the error
 *  int listIdx -- if the argument causing the error is a List, the index of the list element causing 
 *                 the error (0-based; first list element is 0)
 *                 
 * @author 200005201
 *
 */
public class ModelError extends com.ge.research.sadl.reasoner.ModelError {
    public enum ExistingNamePart{PREFIX, NAME, BOTH, NOTAPPLICABLE}

    private Object context;	// the object in the parser output that is host to this error
    private int lineNumber;
    private int lineLength;
    private int offset = -1;
    
    private int argumentIndex;
    private int listIndex;
    private ExistingNamePart existingNameIndex;
    
	public ModelError(String msg, ErrorType typ) {
		super(msg, typ);
    	setArgumentIndex(0);
    	setExistingNameIndex(ExistingNamePart.NOTAPPLICABLE);
    }
	
    public ModelError(String msg, ErrorType etype, String hyperlinkFilename) {
		super(msg, etype, hyperlinkFilename);
    }
    public ModelError(int argIdx, ExistingNamePart extNameIdx, String msg) {
		super(msg, ErrorType.ERROR);
        setArgumentIndex(argIdx);
        setExistingNameIndex(extNameIdx);
    }
    
    public ModelError(int argIdx, ExistingNamePart extNameIdx, String msg, ErrorType typ) {
		super(msg, typ);
        setArgumentIndex(argIdx);
        setExistingNameIndex(extNameIdx);
    }
    
    public ModelError(int argIdx, int lstIdx, ExistingNamePart extNameIdx, String msg) {
		super(msg, ErrorType.ERROR);
        setArgumentIndex(argIdx);
        setListIndex(lstIdx);
        setExistingNameIndex(extNameIdx);
    }
    
    public ModelError(int argIdx, int lstIdx, ExistingNamePart extNameIdx, String msg, ErrorType typ) {
		super(msg, ErrorType.ERROR);
        setArgumentIndex(argIdx);
        setListIndex(lstIdx);
        setExistingNameIndex(extNameIdx);
    }
    
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(context, offset);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (!(obj instanceof ModelError))
            return false;
        ModelError other = (ModelError) obj;
        return Objects.equals(context, other.context) && offset == other.offset;
    }
    
    private void setArgumentIndex(int argumentIndex) {
        this.argumentIndex = argumentIndex;
    }

    public int getArgumentIndex() {
        return argumentIndex;
    }

    private void setListIndex(int listIndex) {
        this.listIndex = listIndex;
    }

    public int getListIndex() {
        return listIndex;
    }

    private void setExistingNameIndex(ExistingNamePart existingNameIndex) {
        this.existingNameIndex = existingNameIndex;
    }

    public ExistingNamePart getExistingNameIndex() {
        return existingNameIndex;
    }

	public void setLineNumber(int lineNumber) {
		this.lineNumber = lineNumber;
	}

	public int getLineNumber() {
		return lineNumber;
	}

	public void setLineLength(int lineLength) {
		this.lineLength = lineLength;
	}

	public int getLineLength() {
		return lineLength;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public int getOffset() {
		return offset;
	}

	public Object getContext() {
		return context;
	}

	public void setContext(Object hostObject) {
		this.context = hostObject;
	}
}
