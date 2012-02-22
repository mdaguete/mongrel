% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(binary(X), mongrel_types:binary(X)).

-define(uuid(X), mongrel_types:uuid(X)).

-define(md5(X), mongrel_types:md5(X)).

-define(to_map(Record), {Record, record_info(fields, Record)}).
