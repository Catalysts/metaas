/*
 * ASTASType.java
 * 
 * Copyright (c) 2006-2008 David Holroyd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.co.badgersinfoil.metaas.impl;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.dom.DocComment;
import uk.co.badgersinfoil.metaas.dom.ASMetaTag;
import uk.co.badgersinfoil.metaas.dom.ASMethod;
import uk.co.badgersinfoil.metaas.dom.ASType;
import uk.co.badgersinfoil.metaas.dom.Visibility;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;

public abstract class ASTASType extends ASTScriptElement implements ASType {
	public ASTASType(LinkedListTree ast) {
		super(ast);
	}

	public String getName() {
		ASTIterator i = new ASTIterator(ast);
		return i.find(AS3Parser.IDENT).getText();
	}

	public void setName(String name) {
		ASTIterator i = new ASTIterator(ast);
		i.find(AS3Parser.IDENT);
		i.replace(ASTUtils.newAST(AS3Parser.IDENT, name));
	}
	
	public List getMethods() {
		List results = new LinkedList();
		for (ASTIterator i=blockIter(); i.hasNext(); ) {
			LinkedListTree member = i.next();
			if (member.getType() == AS3Parser.METHOD_DEF) {
				results.add(new ASTASMethod(member));
			}
		}
		return Collections.unmodifiableList(results);
	}

	/**
	 * Returns the ActionScript method with the given name, or null, if no
	 * such method exists.
	 */
	public ASMethod getMethod(String name) {
		// TODO: does AS3 do overloading?  This method will be no use
		//       if it does.
		for (ASTIterator i=blockIter(); i.hasNext(); ) {
			LinkedListTree member = i.next();
			if (member.getType() == AS3Parser.METHOD_DEF) {
				ASMethod meth = new ASTASMethod(member);
				if (meth.getName().equals(name)) {
					return meth;
				}
			}
		}
		return null;
	}

	public void removeMethod(String name) {
		// TODO: does AS3 do overloading?  This method will be no use
		//       if it does.
		for (ASTIterator i=blockIter(); i.hasNext(); ) {
			LinkedListTree member = i.next();
			if (member.getType() == AS3Parser.METHOD_DEF) {
				ASMethod meth = new ASTASMethod(member);
				if (meth.getName().equals(name)) {
					i.remove();
					return;
				}
			}
		}
	}

	protected ASTIterator blockIter() {
		return new ASTIterator(findTypeBlock());
	}

	protected LinkedListTree findTypeBlock() {
		return ASTUtils.findChildByType(ast, AS3Parser.TYPE_BLOCK);
	}

	public void setDocComment(String text) {
		DocCommentUtils.setDocComment(ast, text);
	}

	public String getDocComment() {
		return DocCommentUtils.getDocComment(ast);
	}

	public String getDescriptionString() {
		return getDocumentation().getDescriptionString();
	}

	public void setDescription(String description) {
		getDocumentation().setDescriptionString(description);
	}

	public DocComment getDocumentation() {
		return DocCommentUtils.createDocComment(ast);
	}

	public Visibility getVisibility() {
		return ModifierUtils.getVisibility(findModifiers());
	}

	public void setVisibility(Visibility visibility) {
		ModifierUtils.setVisibility(findModifiers(), visibility);
	}

	private LinkedListTree findModifiers() {
		return ASTUtils.findChildByType(ast, AS3Parser.MODIFIERS);
	}

	public List getAllMetaTags() {
		return TagUtils.getAllMetaTags(ast);
	}

	public ASMetaTag getFirstMetatag(String name) {
		return TagUtils.getFirstMetaTag(ast, name);
	}

	public List getMetaTagsWithName(String name) {
		return TagUtils.getMetaTagWithName(ast, name);
	}

	public ASMetaTag newMetaTag(String name) {
		return TagUtils.newMetaTag(ast, name);
	}
}