/*
 * TreeTokenListUpdateDelegate.java
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

package uk.co.badgersinfoil.metaas.impl.antlr;

public interface TreeTokenListUpdateDelegate {
	public void addedChild(LinkedListTree parent, LinkedListTree child);
	public void addedChild(LinkedListTree parent, int index, LinkedListTree child);
	public void appendToken(LinkedListTree parent, LinkedListToken append);
	public void addToken(LinkedListTree parent, int index, LinkedListToken append);
	public void deletedChild(LinkedListTree parent, int index, LinkedListTree child);
	public void replacedChild(LinkedListTree tree, int index, LinkedListTree child, LinkedListTree oldChild);
}
