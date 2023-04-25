package com.project.demo.service.impl;

import com.project.demo.persistence.PointOfSales;
import com.project.demo.persistence.repository.PosRepositoryCustom;
import com.project.demo.rest.dto.PointOfSalesDto;
import com.project.demo.service.WebPosService;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import java.util.*;


@Service
public class WebPosServiceImpl implements WebPosService {
	@Autowired
	private PosRepositoryCustom posRepositoryCustom;
	@Override
	@Nonnull
	@Transactional(readOnly = true)
	public Page<PointOfSales> getEnabledWorkingPosPage(@Nullable String merchantId,
	                                                   @Nullable String posName,
	                                                   @Nullable String operationArea,
	                                                   @Nullable String area,
	                                                   @Nullable String city,
	                                                   @Nullable Boolean online,
	                                                   @Nonnull Set<String> onlinePoses,
	                                                   @Nonnull String lang,
	                                                   @Nonnull Pageable pageable) {
		if (Boolean.TRUE.equals(online) && onlinePoses.isEmpty()) {
			return new PageImpl<>(Collections.emptyList(), Pageable.unpaged(), 0);
		}

		if (online == null || (Boolean.FALSE.equals(online) && onlinePoses.isEmpty())) {
			return posRepositoryCustom.getEnabledWorkingPosPage(merchantId, posName, operationArea, area, city, lang, pageable);
		} else {
			return posRepositoryCustom.getEnabledWorkingPosPageWithOnline(merchantId, posName, operationArea, area, city, online, onlinePoses, lang, pageable);
		}
	}

}
