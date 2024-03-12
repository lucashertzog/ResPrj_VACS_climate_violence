# Load the DiagrammeR package
library(DiagrammeR)

# Create the flowchart
grViz("
digraph flowchart {
    # Define global attributes
    graph [fontname = Helvetica, label='VACS El Salvador 2017', fontsize=22, labelloc=t, labeljust=c]
    node [fontname = Helvetica, shape = box]
    edge [fontname = Helvetica]
    
    # Define nodes
    q600 [label='Q600: Touched in a sexual way without permission?']
    yes [label='Yes (119)']
    no [label='No (2,303)', style='filled']
    dk_declined [label='DK/Declined (14)']
    
    follow_up [label='Follow up questions', style='filled']
    
    q700a [label='Q700a: Attempted forced sex by partner?', style='filled']
    yes_q700a [label='Yes (20)']
    no_q700a [label='No (1,433)', style='filled']
    dk_q700a [label='DK/Declined (6)']
    
    q700b [label='Q700b: Attempted forced sex by others?', style='filled']
    yes_q700b [label='Yes (31)']
    no_q700b [label='No (2,271)', style='filled']
    dk_q700b [label='DK/Declined (8)']
    
    q800a [label='Q800a: Forced sex by partner?', style='filled']
    yes_q800a [label='Yes (7)']
    no_q800a [label='No (1,446)', style='filled']
    dk_q800a [label='DK/Declined (5)']
    
    q800b [label='Q800b: Forced sex by others?', style='filled']
    yes_q800b [label='Yes (10)']
    no_q800b [label='No (2,290)', style='filled']
    dk_q800b [label='DK/Declined (7)']
    
    follow_up2 [label='Follow up questions', style='filled']
    
    q900a [label='Q900a: Pressured sex by partner?', style='filled']
    yes_q900a [label='Yes (14)']
    no_q900a [label='No (1,538)', style='filled']
    dk_q900a [label='DK/Declined (6)']
    
    q900b [label='Q900b: Pressured sex by others?', style='filled']
    yes_q900b [label='Yes (18)']
    no_q900b [label='No (2,436)', style='filled']
    dk_q900b [label='DK/Declined (13)']


    ### Define edges
    q600 -> yes
    q600 -> no
    q600 -> dk_declined
    no -> follow_up
    dk_declined -> follow_up
    
    follow_up -> q700a
    follow_up -> q700b
    follow_up -> q800a
    follow_up -> q800b
    
    q700a -> yes_q700a
    q700a -> no_q700a
    q700a -> dk_q700a
    
    q700b -> yes_q700b
    q700b -> no_q700b
    q700b -> dk_q700b 
    
    q800a -> yes_q800a
    q800a -> no_q800a
    q800a -> dk_q800a
    
    q800b -> yes_q800b
    q800b -> no_q800b
    q800b -> dk_q800b 
    
    no_q800a -> follow_up2
    no_q800b -> follow_up2
    
    follow_up2 -> q900a
    follow_up2 -> q900b
    
    q900a -> yes_q900a
    q900a -> no_q900a
    q900a -> dk_q900a
    
    q900b -> yes_q900b
    q900b -> no_q900b
    q900b -> dk_q900b
    

} 
")

