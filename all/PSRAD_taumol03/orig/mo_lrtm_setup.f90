
    MODULE mo_lrtm_setup
    USE mo_rrtm_params, ONLY : nbndlw

    INTEGER, PARAMETER :: nspa(nbndlw) = (/ & !< Number of reference atmospheres for lower atmosphere
        1,1,9,9,9,1,9,1,9,1,1,9,9,1,9,9/)

    INTEGER, PARAMETER :: nspb(nbndlw) = (/ & !< Number of reference atmospheres for upper atmosphere
        1,1,5,5,5,0,1,1,1,1,1,0,0,1,0,0/)

    END MODULE mo_lrtm_setup
