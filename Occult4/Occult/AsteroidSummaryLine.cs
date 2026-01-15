using System;
using System.Text;

namespace Occult
{
	internal class AsteroidSummaryLine : IComparable
	{
		public static int SortField;

		public static bool UseCombinedMagnitude;

		private double jDEventDate;

		private double eventHour;

		private double eventMin;

		private double asteroidDiameter;

		private double diameterUncertainty;

		private double asteroidDiameterArcSec;

		private double asteroidRate = 1.0;

		private double minEarthSep;

		private double errorEarthRadii;

		private double maxDurn;

		private double mdrop;

		private double mdropRed;

		private double mv;

		private double magCombined;

		private double sunElong;

		private double planetIllum;

		private double alt;

		private double azimuth;

		private double minD = -1.0;

		private double dkm = -1.0;

		private double sunAlt;

		private double moonAlt;

		private double probability;

		private double sunL90;

		private double l90;

		private double moonIllumination_percent;

		private double moonElongation_deg;

		private double ruwe;

		private string uTDate;

		private string starNo;

		private string asteroidNumber;

		private string asteroidName;

		private string asteroidClass;

		private string raDec;

		private string planetEventTimes;

		private int recordNumber;

		private int moons;

		private int rings;

		private int doubleStarFlag;

		private int nearbyStars;

		private bool isAsteroid;

		private bool isPlanet;

		private bool isPlanetaryMoon;

		private bool planetTimes;

		private bool global;

		private bool isChecked = true;

		private bool hasShapeModel;

		private bool forJWST;

		private bool magAdjusted;

		private StringBuilder EventLine;

		public bool IsChecked
		{
			get
			{
				return isChecked;
			}
			set
			{
				isChecked = value;
			}
		}

		public double Alt
		{
			set
			{
				alt = value;
			}
		}

		public double Azimuth
		{
			set
			{
				azimuth = value;
			}
		}

		public double AsteroidDiameter
		{
			set
			{
				asteroidDiameter = value;
			}
		}

		public double AsteroidRate
		{
			set
			{
				asteroidRate = value;
			}
		}

		public double DiameterUncertainty
		{
			set
			{
				diameterUncertainty = value;
			}
		}

		public double AsteroidDiameterArcSec
		{
			set
			{
				asteroidDiameterArcSec = value;
			}
		}

		public string AsteroidName
		{
			get
			{
				return asteroidName;
			}
			set
			{
				asteroidName = value;
			}
		}

		public string AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
			}
		}

		public double Dkm
		{
			set
			{
				dkm = value;
			}
		}

		public int DoubleStarFlag
		{
			get
			{
				return doubleStarFlag;
			}
			set
			{
				doubleStarFlag = value;
			}
		}

		public double ErrorEarthRadii
		{
			set
			{
				errorEarthRadii = value;
			}
		}

		public double EventHour
		{
			set
			{
				eventHour = value;
			}
		}

		public double EventMin
		{
			set
			{
				eventMin = value;
			}
		}

		public double PlanetIllum
		{
			set
			{
				planetIllum = value;
			}
		}

		public bool HasShapeModel
		{
			set
			{
				hasShapeModel = value;
			}
		}

		public double JDEventDate
		{
			set
			{
				jDEventDate = value;
			}
		}

		public double L90
		{
			set
			{
				l90 = value;
			}
		}

		public double MaxDurn
		{
			set
			{
				maxDurn = value;
			}
		}

		public double MagCombined
		{
			set
			{
				magCombined = value;
			}
		}

		public int NearbyStars
		{
			set
			{
				nearbyStars = value;
			}
		}

		public bool MagAdjusted
		{
			set
			{
				magAdjusted = value;
			}
		}

		public double Mdrop
		{
			set
			{
				mdrop = value;
			}
		}

		public double MdropRed
		{
			set
			{
				mdropRed = value;
			}
		}

		public double MinD
		{
			set
			{
				minD = value;
			}
		}

		public double MinEarthSep
		{
			set
			{
				minEarthSep = value;
			}
		}

		public int Moons
		{
			set
			{
				moons = value;
			}
		}

		public int Rings
		{
			set
			{
				rings = value;
			}
		}

		public string AsteroidClass
		{
			set
			{
				asteroidClass = value;
			}
		}

		public double MoonIllumination_percent
		{
			set
			{
				moonIllumination_percent = value;
			}
		}

		public double MoonElongation_deg
		{
			set
			{
				moonElongation_deg = value;
			}
		}

		public double Mv
		{
			set
			{
				mv = value;
			}
		}

		public string PlanetEventTimes
		{
			set
			{
				planetEventTimes = value;
			}
		}

		public double Probability
		{
			get
			{
				return probability;
			}
			set
			{
				probability = value;
			}
		}

		public string RADec
		{
			set
			{
				raDec = value;
			}
		}

		public int RecordNumber
		{
			set
			{
				recordNumber = value;
			}
		}

		public string StarNo
		{
			get
			{
				return starNo;
			}
			set
			{
				starNo = value;
			}
		}

		public double RUWE
		{
			get
			{
				return ruwe;
			}
			set
			{
				ruwe = value;
			}
		}

		public double SunAlt
		{
			set
			{
				sunAlt = value;
			}
		}

		public double SunElong
		{
			set
			{
				sunElong = value;
			}
		}

		public double SunL90
		{
			set
			{
				sunL90 = value;
			}
		}

		public double MoonAlt
		{
			set
			{
				moonAlt = value;
			}
		}

		public string UTDate
		{
			get
			{
				return uTDate;
			}
			set
			{
				uTDate = value;
			}
		}

		public bool ForJWST
		{
			get
			{
				return forJWST;
			}
			set
			{
				forJWST = value;
			}
		}

		public bool IsAsteroid
		{
			set
			{
				isAsteroid = value;
			}
		}

		public bool IsPlanet
		{
			set
			{
				isPlanet = value;
			}
		}

		public bool IsPlanetaryMoon
		{
			set
			{
				isPlanetaryMoon = value;
			}
		}

		public bool PlanetTimes
		{
			set
			{
				planetTimes = value;
			}
		}

		public bool Global
		{
			get
			{
				return global;
			}
			set
			{
				global = value;
			}
		}

		public override string ToString()
		{
			return ToString(Abbreviated: false, UseCombinedMagnitude);
		}

		public string ToString(bool Abbreviated, bool UseCombinedMag)
		{
			EventLine = new StringBuilder();
			if (planetTimes & !Global)
			{
				if (isPlanet & !isPlanetaryMoon)
				{
					EventLine.Append(uTDate.PadRight(13));
					EventLine.AppendFormat("{0,3:F0}", eventHour);
					EventLine.AppendFormat("{0,5:F1}", eventMin);
					EventLine.AppendFormat("{0,6:F0} ", asteroidDiameter);
					EventLine.Append(asteroidName.Trim().PadRight(9));
					EventLine.Append(starNo.PadRight(15));
					EventLine.AppendFormat(" Mag={0,1:F1}", mv);
					EventLine.AppendFormat(" Elong={0,1:F0}°", sunElong);
					EventLine.AppendFormat(" Ill={0,1:F0}%   ", planetIllum);
					EventLine.Append("Local times: ");
					EventLine.Append(planetEventTimes);
					EventLine.AppendFormat("{0,10:F0}", recordNumber);
				}
			}
			else
			{
				EventLine.Append(uTDate.PadRight(13));
				EventLine.AppendFormat("{0,3:F0}", eventHour);
				EventLine.AppendFormat("{0,5:F1}", eventMin);
				if (asteroidDiameter < 4.0)
				{
					EventLine.AppendFormat("{0,6:F2}", asteroidDiameter);
				}
				else if (asteroidDiameter < 9.95)
				{
					EventLine.AppendFormat("{0,6:F1}", asteroidDiameter);
				}
				else
				{
					EventLine.AppendFormat("{0,6:F0}", asteroidDiameter);
				}
				EventLine.Append(string.Format(" {0,5:F3}", asteroidDiameterArcSec).Substring(0, 6));
				if (maxDurn < 2.0)
				{
					EventLine.AppendFormat("{0,6:F2}s ", maxDurn);
				}
				else if (maxDurn < 100.0)
				{
					EventLine.AppendFormat("{0,6:F1}s ", maxDurn);
				}
				else if (maxDurn < 1000.0)
				{
					EventLine.AppendFormat("{0,6:F0}s ", maxDurn);
				}
				else if (maxDurn < 6000.0)
				{
					EventLine.AppendFormat("{0,6:F1}m ", maxDurn / 60.0);
				}
				else
				{
					EventLine.AppendFormat("{0,6:F0}m ", maxDurn / 60.0);
				}
				if (asteroidRate < 3.0)
				{
					EventLine.AppendFormat("{0,4:F2} ", asteroidRate);
				}
				else if (asteroidRate < 99.5)
				{
					EventLine.AppendFormat("{0,4:F1} ", asteroidRate);
				}
				else
				{
					EventLine.AppendFormat("{0,3:F0}  ", asteroidRate);
				}
				if (UseCombinedMag)
				{
					EventLine.AppendFormat("{0,4:F1}", magCombined);
				}
				else
				{
					EventLine.AppendFormat("{0,4:F1}", mv);
				}
				if (mdrop >= 0.5)
				{
					EventLine.AppendFormat("{0,5:F1}", mdrop);
				}
				else
				{
					EventLine.AppendFormat("{0,5:F2}", mdrop);
				}
				if ((mdropRed >= 20.0) | (mdropRed < 0.005))
				{
					EventLine.Append("".PadRight(5));
				}
				else if (mdropRed >= 0.5)
				{
					EventLine.AppendFormat("{0,5:F1}", mdropRed);
				}
				else
				{
					EventLine.AppendFormat("{0,5:F2}", mdropRed);
				}
				if (nearbyStars > 0)
				{
					if (magAdjusted)
					{
						EventLine.Append("‡");
					}
					else
					{
						EventLine.Append("†");
					}
				}
				else
				{
					EventLine.Append(" ");
				}
				EventLine.AppendFormat("{0,4:F0}", sunElong);
				if (isPlanet & !isPlanetaryMoon)
				{
					EventLine.AppendFormat("{0,4:F0} ", planetIllum);
				}
				else
				{
					EventLine.Append("     ");
				}
				EventLine.Append(starNo.PadRight(19));
				EventLine.Append(" WDDVVVV        Kswddvvvv        k".Substring(doubleStarFlag, 1));
				if (RUWE > 0.0)
				{
					EventLine.Append(" " + string.Format("{0,4:f2}", RUWE).Substring(0, 4) + " ");
				}
				else
				{
					EventLine.Append("      ");
				}
				EventLine.Append(asteroidNumber.PadLeft(11).PadRight(12));
				EventLine.Append(asteroidName.PadRight(16).Substring(0, 16));
				if (!Abbreviated)
				{
					if (!global)
					{
						if (forJWST)
						{
							EventLine.Append(string.Format(" JWST {0,4:F0} ... ±{1,1:f0} km", minEarthSep * 6378.137, errorEarthRadii * 6378.137).PadRight(11));
						}
						else
						{
							EventLine.AppendFormat("{0,4:F0}", alt);
							EventLine.AppendFormat("{0,4:F0}", azimuth);
							if (ForJWST)
							{
								EventLine.AppendFormat("{0,6:F0}", minD * 6378.137);
							}
							else if (minD >= 0.0)
							{
								EventLine.AppendFormat("{0,6:F1}", minD);
							}
							else
							{
								EventLine.AppendFormat("{0,6:F0}", dkm);
							}
							if (sunAlt > -12.0)
							{
								EventLine.AppendFormat("{0,4:F0}", sunAlt);
							}
							else
							{
								EventLine.Append("    ");
							}
							if (moonAlt > -3.0)
							{
								EventLine.AppendFormat("{0,4:F0}", moonAlt);
							}
							else
							{
								EventLine.Append("    ");
							}
							EventLine.AppendFormat(" {0,5:F0}% ", probability);
						}
					}
					else if (isPlanet & !isPlanetaryMoon)
					{
						EventLine.AppendFormat("{0,4:F0}°", sunL90);
						EventLine.AppendFormat("-{0,4:F0}°", l90);
					}
					else if (ForJWST)
					{
						EventLine.Append(string.Format("{0,4:F0} ±{1,1:f0} km", minEarthSep * 6378.137, errorEarthRadii * 6378.137).PadRight(11));
					}
					else
					{
						EventLine.Append(string.Format("{0,5:F2}", minEarthSep).Substring(0, 5) + " ±" + string.Format("{0,4:F2}", errorEarthRadii).Substring(0, 4));
					}
					EventLine.AppendFormat("{0,5:F0}", moonElongation_deg);
					EventLine.AppendFormat("{0,4:F0} ", moonIllumination_percent);
					if (hasShapeModel)
					{
						EventLine.Append("§");
					}
					else
					{
						EventLine.Append(" ");
					}
					if (moons > 0)
					{
						EventLine.Append("☾");
					}
					else if (asteroidClass == "Binary")
					{
						EventLine.Append("G");
					}
					else
					{
						EventLine.Append(" ");
					}
					if (rings > 0)
					{
						EventLine.Append("◉");
					}
					else if (isPlanet & !isPlanetaryMoon)
					{
						if ("jupiter saturn uranus neptune".Contains(asteroidName.Trim().ToLower()))
						{
							EventLine.Append("◉");
						}
						else
						{
							EventLine.Append(" ");
						}
					}
					else
					{
						EventLine.Append(" ");
					}
					EventLine.Append(raDec);
				}
				EventLine.AppendFormat("{0,10:F0}", recordNumber);
			}
			return EventLine.ToString();
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
			case 1:
				if (asteroidDiameter == ((AsteroidSummaryLine)other).asteroidDiameter)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).asteroidDiameter.CompareTo(asteroidDiameter);
			case 2:
				if (asteroidDiameterArcSec == ((AsteroidSummaryLine)other).asteroidDiameterArcSec)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).asteroidDiameterArcSec.CompareTo(asteroidDiameterArcSec);
			case 3:
				if (maxDurn == ((AsteroidSummaryLine)other).maxDurn)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).maxDurn.CompareTo(maxDurn);
			case 4:
				if (mv == ((AsteroidSummaryLine)other).mv)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return mv.CompareTo(((AsteroidSummaryLine)other).mv);
			case 5:
				if (mdrop == ((AsteroidSummaryLine)other).mdrop)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).mdrop.CompareTo(mdrop);
			case 6:
				if (sunElong == ((AsteroidSummaryLine)other).sunElong)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).sunElong.CompareTo(sunElong);
			case 7:
			{
				int num = string.Compare(starNo, ((AsteroidSummaryLine)other).StarNo);
				if (num == 0)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return num;
			}
			case 8:
				if (asteroidNumber == ((AsteroidSummaryLine)other).asteroidNumber)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return asteroidNumber.CompareTo(((AsteroidSummaryLine)other).asteroidNumber);
			case 9:
			{
				int num = string.Compare(asteroidName, ((AsteroidSummaryLine)other).asteroidName);
				if (num == 0)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return num;
			}
			case 10:
				if (Probability == ((AsteroidSummaryLine)other).Probability)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).Probability.CompareTo(Probability);
			case 11:
				if (minD < 0.0)
				{
					if (dkm == ((AsteroidSummaryLine)other).dkm)
					{
						return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
					}
					return dkm.CompareTo(((AsteroidSummaryLine)other).dkm);
				}
				if (minD == ((AsteroidSummaryLine)other).minD)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return minD.CompareTo(((AsteroidSummaryLine)other).minD);
			case 12:
				if ((mdropRed > 50.0) | (((AsteroidSummaryLine)other).mdropRed > 50.0))
				{
					if (mdrop == ((AsteroidSummaryLine)other).mdrop)
					{
						return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
					}
					return ((AsteroidSummaryLine)other).mdrop.CompareTo(mdrop);
				}
				if (mdropRed == ((AsteroidSummaryLine)other).mdropRed)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return ((AsteroidSummaryLine)other).mdropRed.CompareTo(mdropRed);
			case 13:
				if (magCombined == ((AsteroidSummaryLine)other).magCombined)
				{
					return jDEventDate.CompareTo(((AsteroidSummaryLine)other).jDEventDate);
				}
				return magCombined.CompareTo(((AsteroidSummaryLine)other).magCombined);
			default:
				return 0;
			}
		}
	}
}
