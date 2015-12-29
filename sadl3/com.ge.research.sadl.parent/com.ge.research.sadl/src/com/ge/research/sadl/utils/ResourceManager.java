package com.ge.research.sadl.utils;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Enumeration;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class ResourceManager {

    private static final String pluginId = "com.ge.research.sadl.ui";

    /**
     * Method to return the absolute path to a bundle resource given its local path
     *
     * @param relPath
     *
     * @return File in the Bundle
     * @throws IOException
     * @throws URISyntaxException
     */
    public static File getAbsoluteBundlePath(String relPath, String file) throws IOException, URISyntaxException {
        String symbolicName = pluginId;
        Bundle bndl = Platform.getBundle(symbolicName);
        if (bndl != null) {
            Enumeration<URL> en = bndl.findEntries(relPath, file, true);
            while (en != null && en.hasMoreElements()) {
                URL bndlurl = en.nextElement();
                URL fileUrl = FileLocator.toFileURL(bndlurl);
                if (fileUrl != null) {
                	File bundleFile = new File(fileUrl.getFile());
                    if (bundleFile.exists()) {
                        return bundleFile;
                    }
                }
            }
        }
        else {
            try {
                throw new IOException("SADL bundle not found! Unable to get absolute path from relative path '" + relPath + "'");
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        return null;
    }

}
