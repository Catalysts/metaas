/*
 * ASPackage.java
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

package uk.co.badgersinfoil.metaas.dom;

import java.util.List;


/**
 * A package-declaration block, such as <code>package com.example {   }</code>.
 * 
 * @see ASCompilationUnit#getPackage()
 */
public interface ASPackage extends ScriptElement {

	/**
	 * Return the name of this package-block, or null if no name is present.
	 * For example, for this package block:
	 * <pre>package foo.bar {
	 * 	// ...
	 * }</pre>
	 * getName() will return the value "foo.bar".
	 */
	public String getName();

	/**
	 * Sets the name of this package.  To remove the package name, set this
	 * value to null (<em>not</em> an empty string).
	 */
	public void setName(String name);
	
	/**
	 * Returns a reference to the first ASClassType or ASInterfaceType in
	 * this ActionScript package.
	 */
	public ASType getType();

	/**
	 * Returns a list of strings specifying the names which are imported
	 * into this package by package-level import statements.
	 */
	public List findImports();

	/**
	 * Adds an import statement to this package block.
	 */
	public void addImport(String name);

	/**
	 * Removes an import statement from this package block.
	 */
	public boolean removeImport(String name);
}