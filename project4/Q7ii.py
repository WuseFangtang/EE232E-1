mv_actor_list_trim = pickle.load(open('/home/kgicmd/pj4/mv_actor_list_trim.pkl', 'rb'))
def write_to_files(i):
#for i in range(len(mv_actor_list_trim)):
    line = ''
    for j in range(i+1,len(mv_actor_list_trim)):
        inter_set = set(mv_actor_list_trim[i])& set(mv_actor_list_trim[j])
        union_set = set(mv_actor_list_trim[i])| set(mv_actor_list_trim[j])
        card_inter = len(inter_set)
        card_union = len(union_set)
        if(card_inter != 0):
            # write to movie_edge_list file
            weight1 = card_inter / (card_union - 2)# i -> j
            
            line2 = mv_actor_list_trim[i][0] + '\t' \
                    + mv_actor_list_trim[j][0] + '\t' \
                    + str(weight1) + '\n'
                    
            line = line + line2
    if (line != ''):
        return(line)
    else:
        return(0)
            
      
def multicore_write(func):
    f = open('movie_edge_list.txt','w', encoding="ISO-8859-1")
    pool = mp.Pool(processes=6)
    for y in tqdm(pool.imap(func, np.arange(len(mv_actor_list_trim)))):
        if (y != 0):
            f.write(y)
    f.close()
    return 0

multicore_write(write_to_files)
