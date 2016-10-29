library(dplyr)
library(data.table)

sessions_action_detail <- read.csv("sessions_action_detail.csv", stringsAsFactors = FALSE)
sessions_action <- read.csv("sessions_action.csv", stringsAsFactors = FALSE)
sessions_duplicate <- read.csv("sessions_duplicate.csv", stringsAsFactors = FALSE)
sessions_action <- rbind(sessions_action, sessions_duplicate)

sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("apply_coupon",
																				"apply_coupon_click",
                                                                                "apply_coupon_click_success",
                                                                                "apply_coupon_error",
                                                                                "coupon_code_click","coupon_field_focus")] <- "coupon"
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("cancellation_policies"
																				,"cancellation_policy",
																				"cancellation_policy_click")] <- "cancel"
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("guest_cancellation",
																				"guest_itinerary",
                                                                                 "guest_receipt")] <- "guest"
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("host_guarantee",
																				"host_home", "host_refund_guest","host_respond", "host_respond_page","host_standard_suspension",
                                                                                 "click_about_host", "contact_host","change_contact_host_dates" )] <- "host"
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("listing_descriptions",
                                                                                 "listing_recommendations",
                                                                                 "listing_reviews","listing_reviews_page", "manage_listing","view_listing",
                                                                                 "list_your_space","similar_listings",
                                                                                 "update_listing_description" ,"update_listing",  
                                                                                 "user_listings","create_listing", "delete_listing_description","delete_listing",   
                                                                                 "your_listings" )] <- "listing"
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("lookup_message_thread",
																				"message_inbox",
                                                                                 "message_post","message_thread",
                                                                                 "message_to_host_change","message_to_host_focus",
                                                                                 "send_message")] <- "message"
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("login","login_page" ,
                                                                                 "login_modal")] <- "login"  
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("signup_login_page","signup" ,
                                                                                 "signup_modal")] <- "signup" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("wishlist_content_update",
																				"user_wishlists",
                                                                                 "email_wishlist_button","email_wishlist",
                                                                                 "friends_wishlists" ,"wishlist" ,
                                                                                 "popular_wishlists" ,"wishlist_note" ,
                                                                                 "airbnb_picks_wishlists")] <- "wishlist"  
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("account_notification_settings",
                                                                                 "account_payout_preferences",
                                                                                 "account_privacy_settings","account_transaction_history",
                                                                                 "set_password","change_password",
                                                                                 "set_password_page","forgot_password",
                                                                                 "user_profile_content_update","user_profile",
                                                                                 "profile_verifications","edit_profile",
                                                                                 "update_user_profile","profile_references",
                                                                                 "header_userpic", "create_user",
                                                                                 "update_user", "user_reviews", "user_languages"  )] <- "account" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("translations",
                                                                                 "translate_listing_reviews" )] <- "translate" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("booking","book_it",
                                                                                 "request_to_book","instant_book",
                                                                                 "complete_booking", "post_checkout_action", 
                                                                                 "payment_instruments","user_tax_forms" )] <- "booking" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("create_alteration_request",
                                                                                 "respond_to_alteration_request","alteration_request",
                                                                                 "alteration_field", "change_or_alter" )] <- "alteration" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c( "unavailable_dates",
																				"trip_availability", 
																				"change_availability" )] <- "trip_availability"  
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c( "calculate_worth", 
																				"place_worth")] <- "place_worth" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("previous_trips", 
																			"your_trips" )] <- "previous_trips" 
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("your_reservations", 																						"reservations")] <- "reservations"  
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("create_phone_numbers", 																					"delete_phone_numbers")] 
																				<- "phone_numbers"  
sessions_action_detail$action_detail[sessions_action_detail$action_detail %in% c("p3", "p5", "p1", "p4", 																					"terms_and_privacy")] <- "terms"  




sessions_action$action[sessions_action$action %in% c('agree_terms_check', 'agree_terms_uncheck', 
													'agree_terms')] <- "agree_terms"
sessions_action$action[sessions_action$action %in% c('ajax_google_translate_description'
													, 'ajax_google_translate_reviews',
													"google_translate", "ajax_google_translate")] 
													<- "google_translate"
sessions_action$action[sessions_action$action %in% c('ajax_payout_edit', 'ajax_payout_options_by_country',
                                                     "payout_preferences", "ayout_update", "payout_delete", 
                                                     "change_default_payout", "ajax_payout_split_edit")] <- "payout"
sessions_action$action[sessions_action$action %in% c('ajax_photo_widget', 'ajax_photo_widget_form_iframe')] <- 														"photo_widget"
sessions_action$action[sessions_action$action %in% c('ajax_referral_banner_experiment_type', 																		'ajax_referral_banner_type')] <- "banner"
sessions_action$action[sessions_action$action %in% c('create', 'create_ach', 'create_multiple', 'create_paypal',
                                                     "create_airbnb" )] <- "create"
sessions_action$action[sessions_action$action %in% c('complete', 'complete_redirect', 'complete_status',
                                                     "payoneer_signup_complete"  )] <- "complete"
sessions_action$action[sessions_action$action %in% c('department', 'departments')] <- "department"
sessions_action$action[sessions_action$action %in% c('verify', 'edit_verification')] <- "verify"
sessions_action$action[sessions_action$action %in% c('email_itinerary_colorbox', 'email_share', 'email_wishlist',
                                                     "email_by_key")] <- "email"
sessions_action$action[sessions_action$action %in% c('friends', 'friends_new', "tell_a_friend")] <- "friends"
sessions_action$action[sessions_action$action %in% c('home_safety_landing', 'home_safety_terms')] <- "home_safety"
sessions_action$action[sessions_action$action %in% c('jumio', 'jumio_redirect', 'jumio_token')] <- "jumio"
sessions_action$action[sessions_action$action %in% c('kba', 'kba_update')] <- "kba"
sessions_action$action[sessions_action$action %in% c('listing', 'listings')] <- "listing"
sessions_action$action[sessions_action$action %in% c('message_to_host_change', 'message_to_host_focus')] <- 														"message_to_host"
sessions_action$action[sessions_action$action %in% c('multi', 'multi_message', 'multi_message_attributes"')] <- 													"multi"
sessions_action$action[sessions_action$action %in% c('qt_reply_v2', 'qt_with', 'qt2')] <- "qt"
sessions_action$action[sessions_action$action %in% c('reviews', 'reviews_new')] <- "reviews"
sessions_action$action[sessions_action$action %in% c('similar_listings', 'similar_listings_v2')] <- 																"similar_listings"
sessions_action$action[sessions_action$action %in% c('show', 'show_code', "show_personalize" )] <- "show"
sessions_action$action[sessions_action$action %in% c('social', 'social_connections', "social-media")] <- "social"
sessions_action$action[sessions_action$action %in% c('terms', 'terms_and_conditions')] <- "terms"
sessions_action$action[sessions_action$action %in% c('transaction_history', 'transaction_history_paginated')] <- 													"transaction_history"
sessions_action$action[sessions_action$action %in% c('travel_plans_current', 
													'travel_plans_previous')] <- "travel_plans"
sessions_action$action[sessions_action$action %in% c('update_cached', 'update_friends_display', 'update_hide_from_search_engines', 'update_notifications',
                                                    "update_reservation_requirements", "update_message", "update_country_of_residence" )] <- "update"
sessions_action$action[sessions_action$action %in% c("change_password","forgot_password" ,
													"set_password"  )] <- "password" 
sessions_action$action[sessions_action$action %in% c("apply_coupon_click_success","apply_coupon_error" ,"apply_coupon_click","coupon_field_focus","coupon_code_click","apply_coupon_error_type"  )] <- "coupon"   
sessions_action$action[sessions_action$action %in% c("p4_refund_policy_terms","p4_terms"  )] <- "p4"    
sessions_action$action[sessions_action$action %in% c("login","login_modal","facebook_auto_login","signup_login" ,
                                                    "zendesk_login_jwt")] <- "login" 
sessions_action$action[sessions_action$action %in% c("recent_reservations"
													, "clear_reservation","apply_reservation", 
													 "reservation")] <- "reservation" 
sessions_action$action[sessions_action$action %in% c("faq_experiment_ids","faq","faq_category",
                                                     "questions","ask_question" )] <- "faq"
sessions_action$action[sessions_action$action %in% c("payment_methods","payment_instruments" )] <- "payment"
sessions_action$action[sessions_action$action %in% c("recommendation_page","recommend","recommendations",  
                                                    "recommended_listings" )] <- "recommend"
sessions_action$action[sessions_action$action %in% c("languages_multiselect", "spoken_languages" )] <- "language"
sessions_action$action[sessions_action$action %in% c("phone_verification_number_sucessfully_submitted",
													"phone_verification_number_submitted_for_sms",
													"phone_verification_phone_number_removed",
													"phone_verification_call_taking_too_long",
													"phone_verification_number_submitted_for_call" ,
													"phone_verification_error" ,"phone_verification_success" ,
													"phone_verification_modal","phone_verification" )] <- "phone_verification" 
sessions_action$action[sessions_action$action %in% c("acculynk_load_pin_pad" ,
													"acculynk_bin_check_success" ,"acculynk_session_obtained",
													"acculynk_pin_pad_inactive")]  <- "acculynk" 
sessions_action$action[sessions_action$action %in% c("weibo_signup_referral_finish"  ,"signup_weibo_referral",
                                                    "signup_weibo" ,"signup_modal" )]  <- "signup" 
sessions_action$action[sessions_action$action %in% c("receipt", "guest_billing_receipt" )]  <- "receipt"     
sessions_action$action[sessions_action$action %in% c("tos_2014","tos_confirm"  )]  <- "tos"    
sessions_action$action[sessions_action$action %in% c("profile_pic", "has_profile_pic")]  <- "profile_pic" 
sessions_action$action[sessions_action$action %in% c("photography_update", "photography",  
                                                    "request_photography" )] <- "photography" 
sessions_action$action[sessions_action$action %in% c("envoy_bank_details_redirect", "envoy_form"  )] <- "envoy" 
sessions_action$action[sessions_action$action %in% c("other_hosting_reviews_first", "hosting_social_proof"
													,"other_hosting_reviews" ,"message_to_host",
													  "this_hosting_reviews", "host_2013",
													  "host_summary", "why_host", "new_host", "locale_from_host" )] <- "host" 
sessions_action$action[sessions_action$action %in% c("cancellation_policies", 
													"cancellation_policy_click", "cancel"  )] <- "cancel" 


#reshape data
sessions_action_detail <- dcast(sessions_action_detail, id ~ action_detail, mean, value.var="count")
sessions_action <- dcast(sessions_action, id ~ action, mean, value.var="count")

sessions_action_detail[is.na(sessions_action_detail)] <- 0
sessions_action[is.na(sessions_action)] <- 0

#load train and test dataset
train_users <- read.csv("data/X_train.csv", stringsAsFactors = FALSE)
test_users <- read.csv("data/X_test.csv", stringsAsFactors = FALSE)

X_train <- subset(train_users, id %in% unique(sessions$id), select=c("id"))
X_train1 <- subset(train_users, id %in% unique(sessions$id))
X_test <- subset(test_users, id %in% unique(sessions$id), select=c("id"))
X_test1 <- subset(test_users, id %in% unique(sessions$id))


# merging features with train and test data
X_train <- merge(X_train, sessions_action, all.x=T, by="id")
X_train <- merge(X_train, sessions_action_type, all.x=T, by="id")
X_train <- merge(X_train, sessions_action_detail, all.x=T, by="id")
X_test <- merge(X_test, sessions_action, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_type, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_detail, all.x=T, by="id")

X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0

# removing variables with less than 4 occurrences
train_ids <- X_train$id
X_train$id <- NULL
X_train <- as.data.frame(subset(X_train, select=c(names(X_train)
                                                  [which(colSums(X_train) > 4)])))
X_train$id <- train_ids

X_test <- X_test[, names(X_train)]

#merge with train and test dataset
X_train <- merge(X_train1, X_train, all.x=T, by="id")
X_test <- merge(X_test1, X_test, all.x=T, by="id")
X_train$id <- NULL
X_test$id <- NULL

targets <- X_train$country_destination
X_train$country_destination <- NULL
X_train$country_destination <- targets


write.csv(X_train, file = "data/Train_NOTDNF.csv", row.names = FALSE)
write.csv(X_test, file = "data/Test_NOTDNF.csv", row.names = FALSE)