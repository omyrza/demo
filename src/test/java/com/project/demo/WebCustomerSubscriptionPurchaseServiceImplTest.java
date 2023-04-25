package com.project.demo;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.NullSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.doReturn;
@ExtendWith(MockitoExtension.class)
public class WebCustomerSubscriptionPurchaseServiceImplTest {

	private static final String TEST_CUSTOMER_ID = "test-customer-id";
	private static final String TEST_OPERATION_AREA = "test-operation-area";
	private static final String TEST_LANG = "test-lang";

	@InjectMocks
	private WebCustomerSubscriptionPurchaseV3ServiceImpl webCustomerSubscriptionPurchaseService;
	@Mock
	private AppCustomerSubscriptionService appCustomerSubscriptionService;
	@Mock
	private CatalogApiService catalogApiService;
	@Mock
	private  SubscriptionsV2DtoMapper subscriptionsV2DtoMapper;
	@Mock
	private  SubscriptionOperationValidator subscriptionOperationValidator;
	@Mock
	private WebSubscriptionOperationMapperV2 operationMapper;
	@Mock
	private  SubscriptionCheckoutMapper subscriptionCheckoutMapper;
	@Mock
	private SubscriptionPojoMapper subscriptionPojoMapper;
	@Mock
	private SettingsService settingsService;
	@Mock
	private WebSubscriptionBannerContentService webSubscriptionBannerContentService;
	@Mock
	private Map<String, String> langMap;
	@Mock
	private StringToStringMapSettingDto stringToStringMap;

	@BeforeEach
	void setUp() {
		webCustomerSubscriptionPurchaseService = new WebCustomerSubscriptionPurchaseV3ServiceImpl(
				appCustomerSubscriptionService,
				catalogApiService,
				subscriptionsV2DtoMapper,
				subscriptionOperationValidator,
				operationMapper,
				subscriptionCheckoutMapper,
				subscriptionPojoMapper,
				settingsService,
				webSubscriptionBannerContentService
		);
	}
	@Test
	void getSubscriptionCheckoutInfo_validates_subscription_when_feature_toggle_is_true() throws BusinessLogicException {
		SubscriptionCheckoutRequestV2Dto subscriptionCheckoutRequestDto = new SubscriptionCheckoutRequestV2Dto();
		SubscriptionOperation subscriptionOperation = new SubscriptionOperation();
		ActiveCustomerSubscriptionPojo activeCustomerSubscription = new ActiveCustomerSubscriptionPojo();
		SubscriptionPojo subscriptionPojo = SubscriptionPojo.builder().build();

		doReturn(subscriptionOperation).when(operationMapper).toSubscriptionOperation(any(), any(), any(), any());
		doReturn(activeCustomerSubscription).when(appCustomerSubscriptionService).getActiveCustomerSubscriptionWithoutCheckingCustomerByOperationArea(eq(TEST_CUSTOMER_ID), any());
		doReturn(subscriptionPojo).when(appCustomerSubscriptionService).getSubscriptionPojo(eq(activeCustomerSubscription), any(), eq(TEST_LANG));

		doReturn(true)
				.when(settingsService)
				.getNonnullValue(SettingDescriptor.FEATURE_TOGGLE_DO_NOT_USE_SUBSCRIPTION_OPERATION_VALIDATOR);

		webCustomerSubscriptionPurchaseService.getSubscriptionCheckoutInfo(subscriptionCheckoutRequestDto, TEST_CUSTOMER_ID, TEST_LANG);
		verify(subscriptionOperationValidator, times(1)).validate(eq(subscriptionOperation));
	}

	@ParameterizedTest
	@CsvSource(value = {
			"SUBSCRIBE",
			"UPGRADE"
	})
	void getSubscriptionCheckoutInfo_returns_subscription_checkout_info_for_SubscriptionOperationType_SUBSCRIBE_or_UPGRADE(SubscriptionCheckoutOperationType type) throws BusinessLogicException {
		SubscriptionCheckoutRequestV2Dto subscriptionCheckoutRequestDto = new SubscriptionCheckoutRequestV2Dto();
		subscriptionCheckoutRequestDto.setType(type);
		SubscriptionOperation subscriptionOperation = new SubscriptionOperation();
		SubscriptionPojo subscriptionPojo = SubscriptionPojo.builder().build();
		SubscriptionCheckoutResponseV3Dto responseDto = new SubscriptionCheckoutResponseV3Dto();
		SubscriptionDetailsDto detailsDto = new SubscriptionDetailsDto();

		doReturn(subscriptionOperation).when(operationMapper).toSubscriptionOperation(any(), any(), any(), any());
		doReturn(detailsDto).when(catalogApiService).getSubscriptionDetailsById(any(), any());
		doReturn(subscriptionPojo).when(subscriptionPojoMapper).mapNotActiveSubscription(eq(detailsDto), eq("link"));
		doReturn(responseDto).when(subscriptionCheckoutMapper)
				.toSubscriptionCheckoutResponseV3Dto(any(), any(), any());
		doReturn(stringToStringMap).when(settingsService).getNonnullValue(eq(SettingDescriptor.LANG_SPECIFIC_SUBSCRIPTIONS_TERMS_CONDITION_URL));
		doReturn(langMap).when(stringToStringMap).getValue();
		doReturn("link").when(langMap).get(any());
		doReturn(true).when(settingsService).getNonnullValue(eq(SettingDescriptor.FEATURE_TOGGLE_ENABLE_LANG_SPECIFIC_SUBSCRIPTION_CONDITIONS_TERMS_LINK_IN_FIN_PRODUCT));

		webCustomerSubscriptionPurchaseService.getSubscriptionCheckoutInfo(subscriptionCheckoutRequestDto, TEST_CUSTOMER_ID, TEST_LANG);

		assertEquals(responseDto, webCustomerSubscriptionPurchaseService
				.getSubscriptionCheckoutInfo(subscriptionCheckoutRequestDto, TEST_CUSTOMER_ID, TEST_LANG));
	}

	@ParameterizedTest
	@EmptySource
	@NullSource
	void getSubscriptionCheckoutInfo_returns_active_subscription_checkout_info_for_SubscriptionOperationType_PROLONGATE(List<String> availableMerchants) throws BusinessLogicException {
		SubscriptionCheckoutRequestV2Dto subscriptionCheckoutRequestDto = new SubscriptionCheckoutRequestV2Dto();
		subscriptionCheckoutRequestDto.setType(SubscriptionCheckoutOperationType.PROLONGATE);
		SubscriptionOperation subscriptionOperation = new SubscriptionOperation();
		ActiveCustomerSubscriptionPojo activeCustomerSubscription = new ActiveCustomerSubscriptionPojo();
		SubscriptionPojo subscriptionPojo = SubscriptionPojo.builder().availableMerchants(availableMerchants).build();
		SubscriptionCheckoutResponseV3Dto responseDto = new SubscriptionCheckoutResponseV3Dto();

		doReturn(subscriptionOperation).when(operationMapper).toSubscriptionOperation(any(), any(), any(), any());
		doReturn(activeCustomerSubscription).when(appCustomerSubscriptionService).getActiveCustomerSubscription(eq(TEST_CUSTOMER_ID), any());
		doReturn(false)
				.when(settingsService)
				.getNonnullValue(SettingDescriptor.FEATURE_TOGGLE_DO_NOT_USE_SUBSCRIPTION_OPERATION_VALIDATOR);

		doReturn(subscriptionPojo).when(appCustomerSubscriptionService).getSubscriptionPojo(eq(activeCustomerSubscription), any(), eq(TEST_LANG));
		doReturn(responseDto).when(subscriptionCheckoutMapper)
				.toSubscriptionCheckoutResponseV3Dto(any(), any(), any());

		assertEquals(responseDto, webCustomerSubscriptionPurchaseService.getSubscriptionCheckoutInfo(subscriptionCheckoutRequestDto, TEST_CUSTOMER_ID, TEST_LANG));
	}

	@Test
	void changeSubscriptionCheckoutInfo_throws_exception_when_active_customer_subscription_is_not_found() {
		SubscriptionCheckoutRequestV2Dto requestDto = new SubscriptionCheckoutRequestV2Dto();
		requestDto.setType(SubscriptionCheckoutOperationType.PROLONGATE);
		requestDto.setOperationArea(TEST_OPERATION_AREA);
		//doNothing().when(catalogApiService).checkOperationAreaExists(eq(TEST_OPERATION_AREA));
		doReturn(null).when(appCustomerSubscriptionService).getActiveCustomerSubscription(TEST_CUSTOMER_ID, TEST_OPERATION_AREA);
		doReturn(false)
				.when(settingsService)
				.getNonnullValue(SettingDescriptor.FEATURE_TOGGLE_DO_NOT_USE_SUBSCRIPTION_OPERATION_VALIDATOR);

		Assertions.assertThatThrownBy(() -> webCustomerSubscriptionPurchaseService.getSubscriptionCheckoutInfo(requestDto, TEST_CUSTOMER_ID, TEST_LANG))
				.isInstanceOf(BusinessLogicException.class)
				.hasMessageContaining(ErrorCode.CUSTOMER_HAS_NO_ACTIVE_SUBSCRIPTION.getKey());
	}

	@ParameterizedTest
	@NullAndEmptySource
	void getSubscriptionCheckoutInfo_throws_server_misconfigured_exception_if_terms_and_conditions_link_is_not_found(Map<String, String> localLangMap) {
		SubscriptionCheckoutRequestV2Dto subscriptionCheckoutRequestDto = new SubscriptionCheckoutRequestV2Dto();
		subscriptionCheckoutRequestDto.setType(SubscriptionCheckoutOperationType.SUBSCRIBE);

		doReturn(stringToStringMap).when(settingsService).getNonnullValue(eq(SettingDescriptor.LANG_SPECIFIC_SUBSCRIPTIONS_TERMS_CONDITION_URL));
		doReturn(localLangMap).when(stringToStringMap).getValue();
		doReturn(true).when(settingsService).getNonnullValue(eq(SettingDescriptor.FEATURE_TOGGLE_ENABLE_LANG_SPECIFIC_SUBSCRIPTION_CONDITIONS_TERMS_LINK_IN_FIN_PRODUCT));

		assertThatThrownBy(() -> webCustomerSubscriptionPurchaseService.getSubscriptionCheckoutInfo(subscriptionCheckoutRequestDto, TEST_CUSTOMER_ID, TEST_LANG))
				.isInstanceOf(ServerMisconfigurationException.class).hasMessageContaining("Mandatory setting " + SettingDescriptor.LANG_SPECIFIC_SUBSCRIPTIONS_TERMS_CONDITION_URL.getKey() + " is undefined.");

	}
}
