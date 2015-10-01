# used to read data from a file containing information on the mortality rate of hospitals across the US
# and return the hosiptal in a chosen state with the lowest mortality rate with respect to a certain 
# disease
best <- function(state, outcome) {
        ## Read outcome data
        outcome_data <-
                read(
                        '~/Documents/Coursera/RProgramming/Assignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv'
                )
        
        ## Read hospital data
        hospital_data <-
                read(
                        '~/Documents/Coursera/RProgramming/Assignment3/rprog_data_ProgAssignment3-data/hospital-data.csv'
                )
        
        ## Check that state is valid
        # first build a set of states combining the list of outcomes and hospitals
        all_states <-
                sort(unique((c(
                        as.character(unique(hospcare$State)), as.character(unique(outcome$State))
                ))))
        
        #then check if supplied state is there in the list and error out if not so
        if (!(state %in% all_states)) {
                stop("invalid state")
        }
        
        # check if supplied outcome is valid
        all_outcomes <- character()
        # we will use the headers from col 11 till end to build the
        # list of all outcomes
        for (ocol in names(outcome_data)[seq(11,ncol(outcome_data))]) {
                # first get the outcome name out
                outcome_col = unlist(strsplit(x = ocol,split = ".from."))[2]
                
                
                # then add to the list
                all_outcomes = c(all_outcomes, outcome_col)
        }
        # get only uniques
        all_outcomes = unique(all_outcomes)
        
        #then check if supplied disease is there in the list and error out if not so
        if (!(outcome %in% tolower(all_outcomes))) {
                stop("invalid outcome")
        }
        
        matched_outcome <- character()
        # which column does it match with
        for (a in all_outcomes) {
                orig_a <- a
                # then remove the periods that occur in the header
                a <- gsub(a, pattern = "\\.", replacement = " ")
                #then convert to lower case
                a <- tolower(a)
                if (tolower(outcome) %in% a) {
                        matched_outcome <- orig_a
                }
        }
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        # first we need to get the death rates out
        mortality_data <-
                subset.data.frame(x = outcome,select = c('State', 'Hospital.Name', names(outcome)[grepl(pattern = "^Hospital.30.Day.Death..Mortality..Rates.from.", x = names(outcome))]))
        
        #split the data by state
        mort_statewise <- split(mortality_data, mortality_data$State)
        
        #get the data for the chosen state
        mort_for_state <- mort_statewise$state
        #get the outcome column header
        matched_mort_col <-
                paste("Hospital.30.Day.Death..Mortality..Rates.from.",matched_outcome,sep = "")
        # we remove the rows that don't have any data
        mort_for_state <-
                mort_for_state[!is.na(as.numeric(as.character(
                        mort_for_state$matched_mort_col
                ))),]
        #sort by mort rate ascending (lower is better) and hospital name alphabetically
        top_hospital <-
                mort_for_state[order(mort_for_state$matched_mort_col, mort_for_state$Hospital.Name),][1,'Hospital.Name']
        
        top_hospital
        
}
