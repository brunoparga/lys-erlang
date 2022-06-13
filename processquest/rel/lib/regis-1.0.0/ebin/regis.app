%% coding: utf-8
{application,regis,
             [{description,"A non-distributed process registry"},
              {vsn,"1.0.0"},
              {id,[]},
              {modules,[regis,regis_server,regis_sup]},
              {registered,[regis_server]},
              {applications,[stdlib,kernel]},
              {optional_applications,[]},
              {included_applications,[]},
              {env,[]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{regis,[]}}]}.

