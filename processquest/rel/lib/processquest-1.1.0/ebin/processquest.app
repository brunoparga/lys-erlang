%% coding: utf-8
{application,processquest,
             [{description,"Game inspired by the Progress Quest game (http://progressquest.com)"},
              {vsn,"1.1.0"},
              {id,[]},
              {modules,[pq_enemy,pq_events,pq_player,pq_quest,pq_stats,
                        processquest]},
              {registered,[pq_supersup]},
              {applications,[stdlib,kernel,regis,crypto]},
              {optional_applications,[]},
              {included_applications,[]},
              {env,[]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{processquest,[]}}]}.

