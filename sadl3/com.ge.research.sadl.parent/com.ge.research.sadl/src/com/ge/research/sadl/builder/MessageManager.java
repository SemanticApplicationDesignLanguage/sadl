package com.ge.research.sadl.builder;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;

public class MessageManager {
	public static enum MessageType { ERROR, WARN, INFO }

	public class HyperlinkInfo {
		private Resource linkResource;
		private int fileLineNumber;
		private int fileOffset;
		private int fileLength;
		private int offsetInLink;
		private int linkLength;
		
		public HyperlinkInfo(Resource res, int lineno, int foffset, int flength) {
			this(res, lineno, foffset, flength, -1, -1);
		}
		
		public HyperlinkInfo(Resource res, int lineno, int foffset, int flength, int offsetInLnk, int lnkLength) {
			linkResource = res;
			fileLineNumber = lineno;
			fileOffset = foffset;
			fileLength = flength;
			offsetInLink = offsetInLnk;
			linkLength = lnkLength;
		}
		
		public Resource getLinkResource() {
			return linkResource;
		}
		
		public int getFileLineNumber() {
			return fileLineNumber;
		}
		
		public int getFileOffset() {
			return fileOffset;
		}
		
		public int getFileLength() {
			return fileLength;
		}

		public int getOffsetInLink() {
			return offsetInLink;
		}

		public int getLinkLength() {
			return linkLength;
		}
	}

	public class SadlMessage {
		private MessageType mtype;
		private String message;
		private HyperlinkInfo linkInfo;
		
		public SadlMessage(MessageType type, String msg, HyperlinkInfo lnkinfo) {
			mtype = type;
			message = msg;
			linkInfo = lnkinfo;
		}
		
		public MessageType getType() {
			return mtype;
		}
		
		public String getMessage() {
			return message;
		}
		
		public HyperlinkInfo getLinkInfo() {
			return linkInfo;
		}
	}
	
	private List<SadlMessage> messages = new ArrayList<SadlMessage>();
	
	private void addMessage(MessageType type, String msg) {
		addMessage(type, msg, null);
	}
	private void addMessage(MessageType type, String msg, HyperlinkInfo lnkinfo) {
		messages.add(new SadlMessage(type, msg, lnkinfo));
	}
	
	public void info(String msg) {
		addMessage(MessageType.INFO, msg);
	}

	public void info(String msg, HyperlinkInfo hyperlinkInfo) {
		addMessage(MessageType.INFO, msg, hyperlinkInfo);
	}

	public void warn(String msg) {
		addMessage(MessageType.WARN, msg);
	}

	public void warn(String msg, HyperlinkInfo hyperlinkInfo) {
        addMessage(MessageType.WARN, msg, hyperlinkInfo);
	}

	public void error(String msg) {
		addMessage(MessageType.ERROR, msg);
	}

	public void error(String msg, HyperlinkInfo hyperlinkInfo) {
        addMessage(MessageType.ERROR, msg, hyperlinkInfo);
	}
	
	public SadlMessage getNextMessage() {
		if (messages.size() > 0) {
			return messages.remove(0);
		}
		return null;
	}
}
