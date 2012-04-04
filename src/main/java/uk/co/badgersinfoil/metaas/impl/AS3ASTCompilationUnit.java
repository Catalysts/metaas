/*
 * AS3ASTCompilationUnit.java
 * 
 * Copyright (c) 2006 David Holroyd
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

import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.dom.ASPackage;
import uk.co.badgersinfoil.metaas.dom.ASType;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;


public class AS3ASTCompilationUnit extends ASTASCompilationUnit {

	public AS3ASTCompilationUnit(LinkedListTree ast) {
		super(ast);
	}

	public String getPackageName() {
		return getPackage().getName();
	}

	public void setPackageName(String pkgName) {
		getPackage().setName(pkgName);
	}

	public ASType getType() {
		ASPackage pkg = getPackage();
		if (pkg == null) {
			return null;
		}
		return pkg.getType();
	}
	
	private LinkedListTree getPkgNode() {
		return ASTUtils.findChildByType(ast, AS3Parser.PACKAGE);
	}

	public ASPackage getPackage() {
		LinkedListTree pkg = getPkgNode();
		if (pkg == null) {
			return null;
		}
		return new ASTASPackage(pkg);
	}
}