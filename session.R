library(dplyr)
library(data.table)

train_data <- read.csv("Data.csv", stringsAsFactors = FALSE)
sessions <- read.csv("data/sessions.csv", stringsAsFactors = FALSE)

names(sessions)[1] <- "id"
sessions1 <- subset(sessions, id %in% train_data$id)
sessions1$count <- 1

#action_type
session.fad <- select(sessions1, id, action_type, count)
session.fat <- subset(session.fad, action_type!='' & action_type!='-unknown-')
sessions_action_type <- dcast(session.fat, id ~ action_type, fun=sum, value.var="count")

#action_detail
session.fad <- select(sessions1, id, action_detail, count)
unique(session.fad$action_detail)

# merging some similar events into a single one
session.fad$action_detail[session.fad$action_detail %in% c( "p3","p1","p5", "p4_terms",
                                                            "p4","p4_refund_policy_terms" ,
                                                            "terms_and_privacy","read_policy_click",
                                                            "oauth_response","oauth_login",
                                                            "notifications", "remove_dashboard_alert",  "change_or_alter","homepage","dashboard",
                                                            "confirm_email_link","request_new_confirm_email",
                                                            "confirm_email","create_phone_numbers",
                                                            "delete_phone_numbers","phone_verification_success",
                                                            "admin_templates","at_checkpoint", 
                                                            "toggle_archived_thread","toggle_starred_thread",
                                                            "special_offer_field" ,"pending"  )] <- ""  
session.fad$action_detail[session.fad$action_detail %in% c("apply_coupon","apply_coupon_click",
                                                           "apply_coupon_click_success","apply_coupon_error",
                                                           "coupon_code_click","coupon_field_focus")] <- "coupon"
session.fad$action_detail[session.fad$action_detail %in% c("cancellation_policies","cancellation_policy",
                                                           "cancellation_policy_click")] <- "cancel"
session.fad$action_detail[session.fad$action_detail %in% c("guest_cancellation","guest_itinerary",
                                                           "guest_receipt")] <- "guest"
session.fad$action_detail[session.fad$action_detail %in% c("host_guarantee","host_home",
                                                           "host_refund_guest","host_respond",
                                                           "host_respond_page","host_standard_suspension",
                                                           "click_about_host", "contact_host","change_contact_host_dates" )] <- "host"
session.fad$action_detail[session.fad$action_detail %in% c("listing_descriptions",
                                                           "listing_recommendations",
                                                           "listing_reviews","listing_reviews_page",
                                                           "manage_listing","view_listing",
                                                           "list_your_space","similar_listings",
                                                           "update_listing_description" ,"update_listing",  
                                                           "user_listings","create_listing",  
                                                           "delete_listing_description","delete_listing",   
                                                           "your_listings" )] <- "listing"
session.fad$action_detail[session.fad$action_detail %in% c("lookup_message_thread","message_inbox",
                                                           "message_post","message_thread",
                                                           "message_to_host_change","message_to_host_focus",
                                                           "send_message")] <- "message"
session.fad$action_detail[session.fad$action_detail %in% c("login","login_page" ,
                                                            "login_modal")] <- "login"  
session.fad$action_detail[session.fad$action_detail %in% c("signup_login_page","signup" ,
                                                           "signup_modal")] <- "signup" 
session.fad$action_detail[session.fad$action_detail %in% c("wishlist_content_update","user_wishlists",
                                                           "email_wishlist_button","email_wishlist",
                                                           "friends_wishlists" ,"wishlist" ,
                                                           "popular_wishlists" ,"wishlist_note" ,
                                                           "airbnb_picks_wishlists")] <- "wishlist"  
session.fad$action_detail[session.fad$action_detail %in% c("account_notification_settings",
                                                           "account_payout_preferences",
                                                           "account_privacy_settings","account_transaction_history",
                                                           "set_password","change_password",
                                                           "set_password_page","forgot_password",
                                                           "user_profile_content_update","user_profile",
                                                           "profile_verifications","edit_profile",
                                                           "update_user_profile","profile_references",
                                                           "header_userpic", "create_user",
                                                           "update_user", "user_reviews", "user_languages"  )] <- "account" 
session.fad$action_detail[session.fad$action_detail %in% c("translations",
                                                           "translate_listing_reviews" )] <- "translate" 
session.fad$action_detail[session.fad$action_detail %in% c("booking","book_it",
                                                           "request_to_book","instant_book",
                                                           "complete_booking", "post_checkout_action", 
                                                           "payment_instruments","user_tax_forms" )] <- "booking" 
session.fad$action_detail[session.fad$action_detail %in% c("create_alteration_request",
                                                           "respond_to_alteration_request","alteration_request",
                                                           "alteration_field" )] <- "alteration" 
session.fad$action_detail[session.fad$action_detail %in% c( "unavailable_dates", "trip_availability",
                                                            "change_availability" )] <- "trip_availability"  
session.fad$action_detail[session.fad$action_detail %in% c( "calculate_worth", "place_worth"
)] <- "place_worth" 
session.fad$action_detail[session.fad$action_detail %in% c("previous_trips", "your_trips" 
)] <- "previous_trips" 
session.fad$action_detail[session.fad$action_detail %in% c("your_reservations", "reservations"
)] <- "reservations"  


session.fad <- subset(session.fad, action_detail!='' & action_detail!='-unknown-')
sessions_action_detail <- dcast(session.fad, id ~ action_detail, fun=sum, value.var="count")



#get all if from train dataset
id = select(train_data, id)


train_merge <- merge(id, sessions_action_type, by="id", all = TRUE)
train_merge <- merge(train_merge, sessions_action_detail, by="id", all = TRUE)
train_merge[is.na(train_merge)] <- 0


#merge seesion to train dataset
merge_session <- merge(train_data, train_merge, by="id", all = TRUE)
merge_session$id <- NULL


write.csv(merge_session, file = "mergeSession.csv", row.names = FALSE)
