/*******************************************************************************
 * Copyright (c) 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * $Id: GridDataUtil.java,v 1.1 2014/01/22 20:32:49 crapo Exp $
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Barry Hathaway - modified for CVS imports into SADL
 *******************************************************************************/
package com.ge.research.sadl.ui.imports;

import org.eclipse.swt.layout.GridData;

/**
 * A utility class to simplify the work with 
 * <code>org.eclipse.swt.layout.GridData</code> objects.
 * 
 * @author marcelop
 * @since 0.0.1
 */
public class GridDataUtil
{
	/**
	 * Creates a grid data object that occupies vertical and horizontal space.
	 * @return GridData
	 */
	public static GridData createFill()
	{
		GridData gd= new GridData();
		gd.horizontalAlignment= GridData.FILL;
		gd.grabExcessHorizontalSpace= true;
		gd.verticalAlignment= GridData.FILL;
		gd.grabExcessVerticalSpace= true;
		return gd;
	}
	
	/**
	 * Creates a grid data object that occupies horizontal space.
	 * @return GridData
	 */
	public static GridData createHorizontalFill()
	{
		GridData gd= new GridData();
		gd.horizontalAlignment= GridData.FILL;
		gd.grabExcessHorizontalSpace= true;
		return gd;
	}
	
	/**
	 * Creates a grid data object that occupies vertical space.
	 * @return GridData
	 */
	public static GridData createVerticalFill()
	{
		GridData gd= new GridData();
		gd.verticalAlignment= GridData.FILL;
		gd.grabExcessVerticalSpace= true;
		return gd;
	}

	/**
	 * Returns the grid data's style.
	 * @param gridData
	 * @return int
	 */	
	public static int getStyle(GridData gridData)
	{
		if(gridData == null)
			return 0;
		
		int style = 0;
		
		switch(gridData.verticalAlignment)
		{
			case GridData.BEGINNING:
				style |= GridData.VERTICAL_ALIGN_BEGINNING;
				break;
			case GridData.CENTER:
				style |= GridData.VERTICAL_ALIGN_CENTER;
				break;
			case GridData.FILL:
				style |= GridData.VERTICAL_ALIGN_FILL;
				break;
			case GridData.END:
				style |= GridData.VERTICAL_ALIGN_END;
				break;
		}

		switch(gridData.horizontalAlignment)
		{
			case GridData.BEGINNING:
				style |= GridData.HORIZONTAL_ALIGN_BEGINNING;
				break;
			case GridData.CENTER:
				style |= GridData.HORIZONTAL_ALIGN_CENTER;
				break;
			case GridData.FILL:
				style |= GridData.HORIZONTAL_ALIGN_FILL;
				break;
			case GridData.END:
				style |= GridData.HORIZONTAL_ALIGN_END;
				break;
		}
		
		if(gridData.grabExcessVerticalSpace)
			style |= GridData.GRAB_VERTICAL;

		if(gridData.grabExcessHorizontalSpace)
			style |= GridData.GRAB_HORIZONTAL;
			
		return style;
	}
	
	/**
	 * Clones a grid data.
	 * @param gridData
	 * @return GridData
	 */
	public static GridData clone(GridData gridData)
	{
		GridData clone = new GridData(getStyle(gridData));
		
		clone.heightHint = gridData.heightHint;
		clone.horizontalIndent = gridData.horizontalIndent;
		clone.horizontalSpan = gridData.horizontalSpan;
		clone.verticalSpan = gridData.verticalSpan;
		clone.widthHint = gridData.widthHint;
		
		return clone;
	}
}
