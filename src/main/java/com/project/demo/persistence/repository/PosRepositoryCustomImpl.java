package com.project.demo.persistence.repository;

import com.project.demo.persistence.PointOfSales;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import java.util.*;
@Repository
public class PosRepositoryCustomImpl implements PosRepositoryCustom {

	@PersistenceContext
	private EntityManager em;
	@Nonnull
	@Override
	@SuppressWarnings(value = "checkstyle:ParameterNumber")
	public Page<PointOfSales> getEnabledWorkingPosPage(
			@Nullable String merchantId,
			@Nullable String posName,
			@Nullable String operationArea,
			@Nullable String area,
			@Nullable String city,
			@Nonnull String lang,
			@Nonnull Pageable pageable) {
		Pair<String, HashMap<String, Object>> sqlResult = buildPosesSqlV2(merchantId, posName, operationArea, area, city, lang, pageable, true);
		String sql = sqlResult.getLeft();
		HashMap<String, Object> params = sqlResult.getRight();
		Query q = em.createNativeQuery(sql);
		JpaUtils.setQueryParams(q, params);
		long total = ((Number) q.getSingleResult()).longValue();

		sqlResult = buildPosesSqlV2(merchantId, posName, operationArea, area, city, lang, pageable, false);
		sql = sqlResult.getLeft();
		params = sqlResult.getRight();
		q = em.createNativeQuery(sql, PointOfSales.class);
		JpaUtils.setQueryParams(q, params);
		List<PointOfSales> pageContent = q.getResultList();

		return new PageImpl<>(pageContent, Pageable.unpaged(), total);
	}

	@SuppressWarnings({"checkstyle:ParameterNumber", "java:S1541", "java:S2259"})
	private Pair<String, HashMap<String, Object>> buildPosesSqlV2(@Nullable String merchantId,
	                                                              @Nullable String name,
	                                                              @Nullable String operationArea,
	                                                              @Nullable String area,
	                                                              @Nullable String city,
	                                                              @Nonnull String lang,
	                                                              @Nonnull Pageable pageable,
	                                                              boolean isTotalSql) {
		HashMap<String, Object> params = new HashMap<>();
		StringBuilder sql;
		if (isTotalSql) {
			sql = new StringBuilder("SELECT count(1) from pos p ");
		} else if (pageable.getSort().isSorted() && pageable.getSort().getOrderFor("name") != null) {
			sql = new StringBuilder("SELECT *, (select pa.value from pos_attribute pa where pa.name='name' and p.id=pa.point_of_sales and lang=:lang limit 1) as name from pos p");
			params.put("lang", lang);
		} else {
			sql = new StringBuilder("SELECT * FROM pos p");
		}

		fillBuilderFilters(merchantId, name, operationArea, area, city, lang, sql, params);

		if (!isTotalSql) {
			sql.append(JpaUtils.buildSort(pageable.getSort()));
			sql.append(" offset " + pageable.getOffset() + " limit " + pageable.getPageSize());
		}
		return ImmutablePair.of(sql.toString(), params);
	}

	private void fillBuilderFilters(@Nullable String merchantId,
	                                @Nullable String name,
	                                @Nullable String operationArea,
	                                @Nullable String area,
	                                @Nullable String city,
	                                @Nonnull String lang,
	                                @Nonnull StringBuilder sql,
	                                @Nonnull HashMap<String, Object> params) {
		sql.append(" WHERE p.enabled=true AND (coalesce(p.archived, false) = false)");

		if (merchantId != null) {
			sql.append(" AND p.merchant=:merchantId ");
			params.put("merchantId", merchantId);
		}

		if (StringUtils.hasLength(operationArea)) {
			sql.append(" AND p.operation_area=:operationArea");
			params.put("operationArea", operationArea);
		}

		if (StringUtils.hasLength(name)) {
			sql.append(" AND EXISTS(SELECT 1 FROM pos_attribute pa WHERE pa.point_of_sales=p.id AND pa.name='name' AND pa.lang=:lang AND LOWER(pa.value) LIKE :name)");
			params.put("name", name.toLowerCase());
			params.put("lang", lang);
		}

		if (StringUtils.hasLength(city)) {
			sql.append(" AND EXISTS(SELECT 1 FROM pos_attribute pa WHERE pa.point_of_sales=p.id AND pa.name='city' AND pa.lang=:lang AND LOWER(pa.value) LIKE :city)");
			params.put("city", city.toLowerCase());
			params.put("lang", lang);
		}
		if (StringUtils.hasLength(area)) {
			sql.append(" AND EXISTS(SELECT 1 FROM pos_attribute pa WHERE pa.point_of_sales=p.id AND pa.name='area' AND pa.lang=:lang AND LOWER(pa.value) LIKE :area)");
			params.put("area", area.toLowerCase());
			params.put("lang", lang);
		}
	}

	@Nonnull
	@Override
	@SuppressWarnings(value = "checkstyle:ParameterNumber")
	public Page<PointOfSales> getEnabledWorkingPosPageWithOnline(@Nullable String merchantId,
	                                                             @Nullable String posName,
	                                                             @Nullable String operationArea,
	                                                             @Nullable String area,
	                                                             @Nullable String city,
	                                                             @Nonnull Boolean online,
	                                                             @Nonnull Set<String> onlinePoses,
	                                                             @Nonnull String lang,
	                                                             @Nonnull Pageable pageable) {
		//get all enabled and filtered sorted list of ids
		Pair<String, HashMap<String, Object>> sqlResult = buildEnabledPosesIdsSql(merchantId, posName, operationArea, area, city, lang, pageable);
		String sql = sqlResult.getLeft();
		HashMap<String, Object> params = sqlResult.getRight();
		Query q = em.createNativeQuery(sql);
		JpaUtils.setQueryParams(q, params);
		List<String> filteredIdsContent = q.getResultList();

		//filter by online pos set
		if (Boolean.TRUE.equals(online)) {
			filteredIdsContent.retainAll(onlinePoses);
		} else if (Boolean.FALSE.equals(online)) {
			filteredIdsContent.removeAll(onlinePoses);
		}

		//Get ids for requested page
		int start = (int) pageable.getOffset();
		int end = Math.min(start + pageable.getPageSize(), filteredIdsContent.size());
		List<String> posIdsPage = new ArrayList<>();
		if (start <= filteredIdsContent.size()) {
			posIdsPage = new ArrayList<>(filteredIdsContent).subList(start, end);
		}

		//get poses for page ids
		List<PointOfSales> filteredPosesPage = new ArrayList<>();
		if (!posIdsPage.isEmpty()) {
			sqlResult = buildPosesListSql(posIdsPage);
			sql = sqlResult.getLeft();
			params = sqlResult.getRight();
			q = em.createNativeQuery(sql, PointOfSales.class);
			JpaUtils.setQueryParams(q, params);
			filteredPosesPage = q.getResultList();
			//Order as in ids array
			List<String> finalPosIdsPage = new ArrayList<>(posIdsPage);
			filteredPosesPage.sort(Comparator.comparing(pos -> finalPosIdsPage.indexOf(pos.getId())));
		}

		return new PageImpl<>(filteredPosesPage, Pageable.unpaged(), filteredIdsContent.size());
	}

	private Pair<String, HashMap<String, Object>> buildEnabledPosesIdsSql(@Nullable String merchantId,
	                                                                      @Nullable String name,
	                                                                      @Nullable String operationArea,
	                                                                      @Nullable String area,
	                                                                      @Nullable String city,
	                                                                      @Nonnull String lang,
	                                                                      @Nonnull Pageable pageable) {
		HashMap<String, Object> params = new HashMap<>();
		StringBuilder sql;
		if (pageable.getSort().isSorted() && pageable.getSort().getOrderFor("name") != null) {
			sql = new StringBuilder("SELECT id FROM (SELECT p.id, (select pa.value from pos_attribute pa where pa.name='name' and p.id=pa.point_of_sales and lang=:lang limit 1) as name from pos p");
			params.put("lang", lang);
		} else {
			sql = new StringBuilder("SELECT p.id FROM pos p");
		}

		fillBuilderFilters(merchantId, name, operationArea, area, city, lang, sql, params);

		sql.append(JpaUtils.buildSort(pageable.getSort()));

		if (pageable.getSort().isSorted() && pageable.getSort().getOrderFor("name") != null) {
			sql.append(") AS id");
		}

		return ImmutablePair.of(sql.toString(), params);
	}

	private Pair<String, HashMap<String, Object>> buildPosesListSql(@Nonnull List<String> posIdsPage) {
		HashMap<String, Object> params = new HashMap<>();
		params.put("posIdsPage", posIdsPage);

		return ImmutablePair.of("SELECT * FROM pos p WHERE p.id IN (:posIdsPage)", params);
	}
}
