/*
 * Environment.scala
 *
 * Copyright 2018 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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

package net.wayfarerx.oversite
package model

import concurrent.ExecutionContext

/**
 * The environment provided to the model.
 *
 * @param compute The context that handles non-blocking operations.
 * @param blocking The context that handles blocking operations.
 * @param assetTypes The types of assets registered with the model.
 */
case class Environment(
  compute: ExecutionContext,
  blocking: ExecutionContext,
  assetTypes: Asset.Types
)
