package com.ge.research.sadl;

import java.io.File;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.mwe.utils.StandaloneSetup;

import com.google.common.collect.Lists;

/**
 * Extended version of MWE's StandaloneSetup. Allows registering of Projects and exclusion patterns
 * for faster execution.
 * 
 * @author thoms
 * @since 13.01.2014
 *
 */
public class StandaloneSetupExt extends StandaloneSetup {
	private static final Log LOG = LogFactory.getLog(StandaloneSetupExt.class);
	private List<Pattern> excludesFolder = Lists.newArrayList();
	private List<Pattern> excludesFile = Lists.newArrayList();
	
	@Override
	public void setPlatformUri(String pathToPlatform) {
		if (!Platform.isRunning()) {
			super.setPlatformUri(pathToPlatform);
		}
	}
	
	public void addExcludeFolder (String regex) {
		try {
			Pattern pattern = Pattern.compile(regex);
			excludesFolder.add(pattern);
		} catch (PatternSyntaxException e) {
			LOG.warn("Exclude pattern ignored: "+e.getMessage());
		}
	}
	
	public void addExcludeFile (String regex) {
		try {
			Pattern pattern = Pattern.compile(regex);
			excludesFile.add(pattern);
		} catch (PatternSyntaxException e) {
			LOG.warn("Exclude pattern ignored: "+e.getMessage());
		}
	}
	
	public void addRegisterProject (String path) {
		if (new File(path).isAbsolute()) {
			registerProject(new File(path+"/.project"));
		} else {
			File projectDir = new File(new File(".").getAbsoluteFile(), path);
			registerProject(new File(projectDir, "/.project"));
		}
	}
	
	@Override
	protected boolean scanFolder(File f, Set<String> visitedPathes) {
		for (Pattern p: excludesFolder) {
			if (p.matcher(f.getPath()).find()) {
				return false;
			}
		}
		return super.scanFolder(f, visitedPathes);
	}
	
	@Override
	protected void registerBundle(File file) {
		for (Pattern p: excludesFile) {
			if (p.matcher(file.getPath()).find()) {
				return;
			}
		}
		super.registerBundle(file);
	}
	
	@Override
	protected void registerProject(File projectFile) {
//		if (Platform.isRunning()) {
//			try {
//				IProjectDescription projectDescription = ResourcesPlugin.getWorkspace().loadProjectDescription(new FileInputStream(projectFile));
//				projectDescription.setLocation(new Path(projectFile.getParentFile().getAbsolutePath()));
//				IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectDescription.getName());
//				if (!project.exists()) {
//					project.create(projectDescription, null);
//				}
//				project.open(null);
//			} catch (Exception e) {
//				throw new RuntimeException(e);
//			}
//		}
		super.registerProject(projectFile);
	}
	
}
