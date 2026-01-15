using System.Collections.Generic;
using System.Text;

namespace Occult
{
	internal class AsteroidPathCoords
	{
		private double longitudeCentre;

		private double longitudeNorth;

		private double longitudeSouth;

		private double longitudeNorthSigma;

		private double longitudeSouthSigma;

		private double longitudeNorthRings;

		private double longitudeSouthRings;

		private double longitudeNorthUmbra;

		private double longitudeSouthUmbra;

		private double latitudeCentre = 100.0;

		private double latitudeNorth = 100.0;

		private double latitudeSouth = 100.0;

		private double latitudeNorthSigma = 100.0;

		private double latitudeSouthSigma = 100.0;

		private double latitudeNorthRings = 100.0;

		private double latitudeSouthRings = 100.0;

		private double latitudeNorthUmbra = 100.0;

		private double latitudeSouthUmbra = 100.0;

		private double altitudeCentre;

		private double altitudeNorth;

		private double altitudeSouth;

		private double altitudeNorthSigma;

		private double altitudeSouthSigma;

		private double altitudeNorthRings;

		private double altitudeSouthRings;

		private double altitudeNorthUmbra;

		private double altitudeSouthUmbra;

		private double tCentre;

		private double starAlt;

		private double starAz;

		private double sunAlt;

		private double tanZ = -1200.0;

		private bool iterateOnT;

		private bool iterateOnLatitude;

		private bool centreLineValid;

		private bool pathIsTopographic;

		private bool pathHasRings;

		private bool showUmbralPath;

		private List<Fences> fences = new List<Fences>();

		public static bool ExtraPrecisionOnTime;

		public List<Fences> Fences
		{
			get
			{
				return fences;
			}
			set
			{
				fences = value;
			}
		}

		public double LongitudeCentre
		{
			get
			{
				return longitudeCentre;
			}
			set
			{
				longitudeCentre = value;
			}
		}

		public double LongitudeLeft
		{
			get
			{
				return longitudeNorth;
			}
			set
			{
				longitudeNorth = value;
			}
		}

		public double LongitudeRight
		{
			get
			{
				return longitudeSouth;
			}
			set
			{
				longitudeSouth = value;
			}
		}

		public double LongitudeLeftSigma
		{
			get
			{
				return longitudeNorthSigma;
			}
			set
			{
				longitudeNorthSigma = value;
			}
		}

		public double LongitudeRightSigma
		{
			get
			{
				return longitudeSouthSigma;
			}
			set
			{
				longitudeSouthSigma = value;
			}
		}

		public double LongitudeLeftRings
		{
			get
			{
				return longitudeNorthRings;
			}
			set
			{
				longitudeNorthRings = value;
			}
		}

		public double LongitudeRightRings
		{
			get
			{
				return longitudeSouthRings;
			}
			set
			{
				longitudeSouthRings = value;
			}
		}

		public double LongitudeLeftUmbra
		{
			get
			{
				return longitudeNorthUmbra;
			}
			set
			{
				longitudeNorthUmbra = value;
			}
		}

		public double LongitudeRightUmbra
		{
			get
			{
				return longitudeSouthUmbra;
			}
			set
			{
				longitudeSouthUmbra = value;
			}
		}

		public double LatitudeCentre
		{
			get
			{
				return latitudeCentre;
			}
			set
			{
				latitudeCentre = value;
			}
		}

		public double LatitudeLeft
		{
			get
			{
				return latitudeNorth;
			}
			set
			{
				latitudeNorth = value;
			}
		}

		public double LatitudeRight
		{
			get
			{
				return latitudeSouth;
			}
			set
			{
				latitudeSouth = value;
			}
		}

		public double LatitudeLeftSigma
		{
			get
			{
				return latitudeNorthSigma;
			}
			set
			{
				latitudeNorthSigma = value;
			}
		}

		public double LatitudeRightSigma
		{
			get
			{
				return latitudeSouthSigma;
			}
			set
			{
				latitudeSouthSigma = value;
			}
		}

		public double LatitudeLeftRings
		{
			get
			{
				return latitudeNorthRings;
			}
			set
			{
				latitudeNorthRings = value;
			}
		}

		public double LatitudeRightRings
		{
			get
			{
				return latitudeSouthRings;
			}
			set
			{
				latitudeSouthRings = value;
			}
		}

		public double LatitudeLeftUmbra
		{
			get
			{
				return latitudeNorthUmbra;
			}
			set
			{
				latitudeNorthUmbra = value;
			}
		}

		public double LatitudeRightUmbra
		{
			get
			{
				return latitudeSouthUmbra;
			}
			set
			{
				latitudeSouthUmbra = value;
			}
		}

		public double AltitudeCentre
		{
			get
			{
				return altitudeCentre;
			}
			set
			{
				altitudeCentre = value;
			}
		}

		public double AltitudeLeft
		{
			get
			{
				return altitudeNorth;
			}
			set
			{
				altitudeNorth = value;
			}
		}

		public double AltitudeRight
		{
			get
			{
				return altitudeSouth;
			}
			set
			{
				altitudeSouth = value;
			}
		}

		public double AltitudeLeftSigma
		{
			get
			{
				return altitudeNorthSigma;
			}
			set
			{
				altitudeNorthSigma = value;
			}
		}

		public double AltitudeRightSigma
		{
			get
			{
				return altitudeSouthSigma;
			}
			set
			{
				altitudeSouthSigma = value;
			}
		}

		public double AltitudeLeftRings
		{
			get
			{
				return altitudeNorthRings;
			}
			set
			{
				altitudeNorthRings = value;
			}
		}

		public double AltitudeRightRings
		{
			get
			{
				return altitudeSouthRings;
			}
			set
			{
				altitudeSouthRings = value;
			}
		}

		public double AltitudeLeftUmbra
		{
			get
			{
				return altitudeNorthUmbra;
			}
			set
			{
				altitudeNorthUmbra = value;
			}
		}

		public double AltitudeRightUmbra
		{
			get
			{
				return altitudeSouthUmbra;
			}
			set
			{
				altitudeSouthUmbra = value;
			}
		}

		public double TCentre
		{
			get
			{
				return tCentre;
			}
			set
			{
				tCentre = value;
			}
		}

		public double StarAlt
		{
			get
			{
				return starAlt;
			}
			set
			{
				starAlt = value;
			}
		}

		public double StarAz
		{
			get
			{
				return starAz;
			}
			set
			{
				starAz = value;
			}
		}

		public double SunAlt
		{
			get
			{
				return sunAlt;
			}
			set
			{
				sunAlt = value;
			}
		}

		public double TanZ
		{
			get
			{
				return tanZ;
			}
			set
			{
				tanZ = value;
			}
		}

		public bool IterateOnT
		{
			get
			{
				return iterateOnT;
			}
			set
			{
				iterateOnT = value;
			}
		}

		public bool IterateOnLatitude
		{
			get
			{
				return iterateOnLatitude;
			}
			set
			{
				iterateOnLatitude = value;
			}
		}

		public bool CentreLineValid
		{
			get
			{
				return centreLineValid;
			}
			set
			{
				centreLineValid = value;
			}
		}

		public bool PathIsTopographic
		{
			get
			{
				return pathIsTopographic;
			}
			set
			{
				pathIsTopographic = value;
			}
		}

		public bool PathHasRings
		{
			get
			{
				return pathHasRings;
			}
			set
			{
				pathHasRings = value;
			}
		}

		public bool ShowUmbralPath
		{
			get
			{
				return showUmbralPath;
			}
			set
			{
				showUmbralPath = value;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("   ");
			if (centreLineValid)
			{
				stringBuilder.Append(Utilities.DEGtoDMS(longitudeCentre, 4, 0, MinutesOnly: false) + "   ");
				stringBuilder.Append(Utilities.DEGtoDMS(latitudeCentre, 3, 0, MinutesOnly: false));
				int num = 0;
				if (ExtraPrecisionOnTime)
				{
					num = 1;
				}
				if (TCentre < 0.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(tCentre + 24.0, 2, 1 + num, MinutesOnly: false).PadLeft(13 + num) + "-".PadLeft(2 - num));
				}
				else if (TCentre >= 24.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(tCentre - 24.0, 2, 1 + num, MinutesOnly: false).PadLeft(13 + num) + "+".PadLeft(2 - num));
				}
				else
				{
					stringBuilder.Append(Utilities.DEGtoDMS(tCentre, 2, 1 + num, MinutesOnly: false).PadLeft(13 + num) + " ".PadLeft(2 - num));
				}
				stringBuilder.AppendFormat("{0,4:F0}", starAlt);
				stringBuilder.AppendFormat("{0,6:F0}", starAz);
				stringBuilder.AppendFormat("{0,6:F0}", sunAlt);
			}
			else if (iterateOnT)
			{
				stringBuilder.Append("  .. .. ..    .. .. ..   .. .. ....    ..    ..    ..");
			}
			else if (iterateOnLatitude)
			{
				stringBuilder.Append("  .. .. ..   ");
				stringBuilder.Append(Utilities.DEGtoDMS(latitudeCentre, 3, 0, MinutesOnly: false));
				stringBuilder.Append("   .. .. ....    ..    ..    ..");
			}
			else
			{
				stringBuilder.Append(Utilities.DEGtoDMS(longitudeCentre, 4, 0, MinutesOnly: false) + "   ");
				stringBuilder.Append(" .. .. ....   .. .. ..    ..    ..    ..");
			}
			if (iterateOnT)
			{
				if (latitudeSouth < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouth, 4, 0, MinutesOnly: false).PadLeft(11));
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouth, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..   .. .. ..");
				}
				if (latitudeNorth < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorth, 4, 0, MinutesOnly: false).PadLeft(11));
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorth, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..   .. .. ..");
				}
				if (latitudeSouthSigma < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouthSigma, 4, 0, MinutesOnly: false).PadLeft(11));
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouthSigma, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..   .. .. ..");
				}
				if (latitudeNorthSigma < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorthSigma, 4, 0, MinutesOnly: false).PadLeft(11));
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorthSigma, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..   .. .. ..");
				}
				if (showUmbralPath)
				{
					if (latitudeSouthUmbra < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouthUmbra, 4, 0, MinutesOnly: false).PadLeft(11));
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouthUmbra, 3, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
					if (latitudeNorthUmbra < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorthUmbra, 4, 0, MinutesOnly: false).PadLeft(11));
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorthUmbra, 3, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
				}
				if (pathHasRings)
				{
					if (latitudeSouthRings < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouthRings, 4, 0, MinutesOnly: false).PadLeft(11));
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouthRings, 3, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..   .. .. ..");
					}
					if (latitudeNorthRings < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorthRings, 4, 0, MinutesOnly: false).PadLeft(11));
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorthRings, 3, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..   .. .. ..");
					}
				}
			}
			else if (iterateOnLatitude)
			{
				if (latitudeSouth < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouth, 4, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..");
				}
				if (latitudeNorth < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorth, 4, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..");
				}
				if (latitudeSouthSigma < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouthSigma, 4, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..");
				}
				if (latitudeNorthSigma < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorthSigma, 4, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("  ... .. ..");
				}
				if (showUmbralPath)
				{
					if (latitudeSouthUmbra < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouthUmbra, 4, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
					if (latitudeNorthUmbra < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorthUmbra, 4, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
				}
				if (pathHasRings)
				{
					if (latitudeSouthRings < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeSouthRings, 4, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
					if (latitudeNorthRings < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(longitudeNorthRings, 4, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
				}
			}
			else
			{
				if (latitudeSouth < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouth, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("   .. .. ..");
				}
				if (latitudeNorth < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorth, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("   .. .. ..");
				}
				if (latitudeSouthSigma < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouthSigma, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("   .. .. ..");
				}
				if (latitudeNorthSigma < 100.0)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorthSigma, 3, 0, MinutesOnly: false).PadLeft(11));
				}
				else
				{
					stringBuilder.Append("   .. .. ..");
				}
				if (showUmbralPath)
				{
					if (latitudeSouthUmbra < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouthUmbra, 4, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
					if (latitudeNorthUmbra < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorthUmbra, 4, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("  ... .. ..");
					}
				}
				if (PathHasRings)
				{
					if (latitudeSouthRings < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeSouthRings, 3, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("   .. .. ..");
					}
					if (latitudeNorthRings < 100.0)
					{
						stringBuilder.Append(Utilities.DEGtoDMS(latitudeNorthRings, 3, 0, MinutesOnly: false).PadLeft(11));
					}
					else
					{
						stringBuilder.Append("   .. .. ..");
					}
				}
			}
			if (tanZ > -1000.0)
			{
				stringBuilder.AppendFormat("{0,7:F2}", tanZ);
			}
			else
			{
				stringBuilder.Append("   ....");
			}
			return stringBuilder.ToString();
		}

		public string ToString_PathForPlot()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,1:F5},", tCentre);
			stringBuilder.AppendFormat("{0,1:F4},", longitudeCentre);
			if (centreLineValid)
			{
				stringBuilder.AppendFormat("{0,1:F4},", latitudeCentre);
			}
			else
			{
				stringBuilder.AppendFormat("{0,1:F4},", 100);
			}
			stringBuilder.AppendFormat("{0,1:F4},", longitudeSouth);
			stringBuilder.AppendFormat("{0,1:F4},", latitudeSouth);
			stringBuilder.AppendFormat("{0,1:F4},", longitudeNorth);
			stringBuilder.AppendFormat("{0,1:F4},", latitudeNorth);
			stringBuilder.AppendFormat("{0,1:F4},", longitudeSouthSigma);
			stringBuilder.AppendFormat("{0,1:F4},", latitudeSouthSigma);
			stringBuilder.AppendFormat("{0,1:F4},", longitudeNorthSigma);
			stringBuilder.AppendFormat("{0,1:F4}", latitudeNorthSigma);
			stringBuilder.AppendFormat(",{0,1:F4},", longitudeSouthUmbra);
			stringBuilder.AppendFormat("{0,1:F4},", latitudeSouthUmbra);
			stringBuilder.AppendFormat("{0,1:F4},", longitudeNorthUmbra);
			stringBuilder.AppendFormat("{0,1:F4}", latitudeNorthUmbra);
			if (PathHasRings)
			{
				stringBuilder.AppendFormat(",{0,1:F4},", longitudeSouthRings);
				stringBuilder.AppendFormat("{0,1:F4},", latitudeSouthRings);
				stringBuilder.AppendFormat("{0,1:F4},", longitudeNorthRings);
				stringBuilder.AppendFormat("{0,1:F4}", latitudeNorthRings);
			}
			return stringBuilder.ToString();
		}
	}
}
