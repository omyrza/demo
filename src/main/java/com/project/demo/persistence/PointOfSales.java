package com.project.demo.persistence;

import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.GenericGenerator;

import java.util.HashMap;
import java.util.Map;

@Entity
@Getter
@Setter
@Table(name = "pos")
public class PointOfSales {
	@Id
	@GeneratedValue(generator = "system-uuid")
	@GenericGenerator(name = "system-uuid", strategy = "uuid2")
	private String id;
	private String externalId;
	@Nonnull
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	@JsonBackReference
	private Merchant merchant;
	@Nonnull
	private GeoPoint geoPoint;
	@Nullable
	@Column(length = 1024)
	private String imageUrl;
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY, orphanRemoval = true)
	@JoinColumn(name = "pointOfSales", referencedColumnName = "id")
	@MapKey(name = "nameLang")
	private Map<NameLang, PointOfSalesAttribute> pointOfSalesAttributes = new HashMap<>();
}
