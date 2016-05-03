package com.ge.research.sadl.actions;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;

abstract public class SadlAction {
	private boolean isCanceled = false;
	protected Method setCanceledMethod;
	private File projectFolder = null;
	private File targetFile = null;
	private Map<String,String> preferenceMap = null;
	
	public boolean isCanceled() {
		return isCanceled;
	}
	
	public void setCanceled(boolean isCanceled) {
		this.isCanceled = isCanceled;
	}
	
	private Map<String,String> getPreferenceMap() {
		return preferenceMap;
	}
	
	protected void setPreferenceMap(Map<String,String> preferenceMap) {
		this.preferenceMap = preferenceMap;
	}
	
	public abstract Object run(IProgressMonitor monitor);

	protected File getProjectFolder() {
		return projectFolder;
	}

	protected void setProjectFolder(File projectFolder) {
		this.projectFolder = projectFolder;
	}

	protected File getTargetFile() {
		return targetFile;
	}

	protected void setTargetFile(File targetFile) {
		this.targetFile = targetFile;
	}

}
