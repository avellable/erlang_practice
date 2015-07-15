-record(event_state, {server, 
                      name="", 
                      to_go=0}).
-record(state,{events, 
               clients}).
-record(event, {name="", 
                description="", 
                pid, 
                timeout={{1970,1,1},{0,0,0}}}).
