using System;
using System.Text;
using Occult.Properties;

namespace Occult
{
	internal class LunarPredictionLine : IComparable
	{
		private const double Radian = 180.0 / Math.PI;

		private double jD;

		private double mv;

		private double mp;

		private double mr;

		private double illumination;

		private double elongation;

		private double sunAlt;

		private double moonAlt;

		private double moonAz;

		private double durn;

		private double pABrightLimb;

		private double cA;

		private double pA;

		private double vA;

		private double aA;

		private double lL;

		private double lB;

		private double lC;

		private double a;

		private double b;

		private double rV;

		private double cCT;

		private double pA_min;

		private double rA;

		private double dec;

		private double rA2000;

		private double dec2000;

		private double limbDistance;

		private double mountainDistance;

		private double grazeLongitude;

		private double grazeLatitude;

		private double grazeLongitude2;

		private double grazeLatitude2 = 100.0;

		private double grazeD;

		private double grazeAZ;

		private double grazeDeltaLat;

		private double n;

		private double moonDistanceKM;

		private double moonSpeedMsec;

		private double illumPlanet;

		private double limbPlanet;

		private double diaPlanet;

		private double phaseAngle;

		private double paPole;

		private double earthDec;

		private int grazeInnerOuterLimit;

		private int elementRecordNumber;

		private int tag;

		private int xZNum;

		private short planet;

		private string eventPhase = "  ";

		private string starID = "";

		private string doubleCode = " ";

		private string variableCode = " ";

		private string waxFlag = "-";

		private string spectrum = "  ";

		private string cusp = "N";

		private string starName;

		private string doubleDetails = "";

		private string variableDetails = "";

		private string doubleObservationsWanted = "";

		private string kepler2ObservationsWanted = "";

		private bool eclipseEvent;

		private bool starNameExists;

		private bool doubleDetailsExist;

		private bool variableDetailsExist;

		private bool doubleWantedExists;

		private bool kepler2WantedExists;

		private bool lightCurveAvailable;

		public static bool IsGrazeLine;

		internal string EventPhase
		{
			get
			{
				return eventPhase;
			}
			set
			{
				eventPhase = value;
			}
		}

		internal string StarId
		{
			get
			{
				return starID;
			}
			set
			{
				starID = value;
			}
		}

		internal int XZnum
		{
			get
			{
				return xZNum;
			}
			set
			{
				xZNum = value;
			}
		}

		internal string DoubleCode
		{
			get
			{
				return doubleCode;
			}
			set
			{
				doubleCode = value;
			}
		}

		internal string VariableCode
		{
			get
			{
				return variableCode;
			}
			set
			{
				variableCode = value;
			}
		}

		internal string WaxFlag
		{
			get
			{
				return waxFlag;
			}
			set
			{
				waxFlag = value;
			}
		}

		internal string Cusp
		{
			get
			{
				return cusp;
			}
			set
			{
				cusp = value;
			}
		}

		internal string Spectrum
		{
			get
			{
				return spectrum;
			}
			set
			{
				spectrum = value;
			}
		}

		internal string ZCName
		{
			get
			{
				return starName;
			}
			set
			{
				starName = value;
				starNameExists = true;
			}
		}

		internal string DoubleDetails
		{
			get
			{
				return doubleDetails;
			}
			set
			{
				doubleDetails = value;
				doubleDetailsExist = true;
			}
		}

		internal string DoubleObservationsWanted
		{
			get
			{
				return doubleObservationsWanted;
			}
			set
			{
				doubleObservationsWanted = value;
				doubleWantedExists = true;
			}
		}

		internal string Kepler2ObservationsWanted
		{
			get
			{
				return kepler2ObservationsWanted;
			}
			set
			{
				kepler2ObservationsWanted = value;
				kepler2WantedExists = true;
			}
		}

		internal string VariableDetails
		{
			get
			{
				return variableDetails;
			}
			set
			{
				variableDetails = value;
				variableDetailsExist = true;
			}
		}

		internal double JD
		{
			get
			{
				return jD;
			}
			set
			{
				jD = value;
			}
		}

		internal double Mv
		{
			get
			{
				return mv;
			}
			set
			{
				mv = value;
			}
		}

		internal double Mp
		{
			get
			{
				return mp;
			}
			set
			{
				mp = value;
			}
		}

		internal double Mr
		{
			get
			{
				return mr;
			}
			set
			{
				mr = value;
			}
		}

		internal double Illumination
		{
			get
			{
				return illumination;
			}
			set
			{
				illumination = value;
			}
		}

		internal double Elongation
		{
			get
			{
				return elongation;
			}
			set
			{
				elongation = value;
			}
		}

		internal double SunAlt
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

		internal double MoonAlt
		{
			get
			{
				return moonAlt;
			}
			set
			{
				moonAlt = value;
			}
		}

		internal double MoonAz
		{
			get
			{
				return moonAz;
			}
			set
			{
				moonAz = value;
			}
		}

		internal double Durn
		{
			get
			{
				return durn;
			}
			set
			{
				durn = value;
			}
		}

		internal double IllumPlanet
		{
			get
			{
				return illumPlanet;
			}
			set
			{
				illumPlanet = value;
			}
		}

		internal double LimbPlanet
		{
			get
			{
				return limbPlanet;
			}
			set
			{
				limbPlanet = value;
			}
		}

		internal double DiaPlanet
		{
			get
			{
				return diaPlanet;
			}
			set
			{
				diaPlanet = value;
			}
		}

		internal double PhaseAngle
		{
			get
			{
				return phaseAngle;
			}
			set
			{
				phaseAngle = value;
			}
		}

		internal double PAPole_deg
		{
			get
			{
				return paPole;
			}
			set
			{
				paPole = value;
			}
		}

		internal double EarthDec_deg
		{
			get
			{
				return earthDec;
			}
			set
			{
				earthDec = value;
			}
		}

		internal short Planet
		{
			get
			{
				return planet;
			}
			set
			{
				planet = value;
			}
		}

		internal double LimbDistance
		{
			get
			{
				return limbDistance;
			}
			set
			{
				limbDistance = value;
			}
		}

		internal double MountainDistance
		{
			get
			{
				return mountainDistance;
			}
			set
			{
				mountainDistance = value;
			}
		}

		internal double PABrightLimb
		{
			get
			{
				return pABrightLimb;
			}
			set
			{
				pABrightLimb = value;
			}
		}

		internal double CA
		{
			get
			{
				return cA;
			}
			set
			{
				cA = value;
			}
		}

		internal double PA
		{
			get
			{
				return pA;
			}
			set
			{
				pA = value;
			}
		}

		internal double VA
		{
			get
			{
				return vA;
			}
			set
			{
				vA = value;
			}
		}

		internal double AA
		{
			get
			{
				return aA;
			}
			set
			{
				aA = value;
			}
		}

		internal double LL
		{
			get
			{
				return lL;
			}
			set
			{
				lL = value;
			}
		}

		internal double LB
		{
			get
			{
				return lB;
			}
			set
			{
				lB = value;
			}
		}

		internal double LC
		{
			get
			{
				return lC;
			}
			set
			{
				lC = value;
			}
		}

		internal double A
		{
			get
			{
				return a;
			}
			set
			{
				a = value;
			}
		}

		internal double B
		{
			get
			{
				return b;
			}
			set
			{
				b = value;
			}
		}

		internal double RV
		{
			get
			{
				return rV;
			}
			set
			{
				rV = value;
			}
		}

		internal double CCT
		{
			get
			{
				return cCT;
			}
			set
			{
				cCT = value;
			}
		}

		internal double dPA_minute
		{
			get
			{
				return pA_min;
			}
			set
			{
				pA_min = value;
			}
		}

		internal double MoonDistanceKM
		{
			get
			{
				return moonDistanceKM;
			}
			set
			{
				moonDistanceKM = value;
			}
		}

		internal double MoonSpeedMsec
		{
			get
			{
				return moonSpeedMsec;
			}
			set
			{
				moonSpeedMsec = value;
			}
		}

		internal double RA
		{
			get
			{
				return rA;
			}
			set
			{
				rA = value;
			}
		}

		internal double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		internal double RA2000
		{
			get
			{
				return rA2000;
			}
			set
			{
				rA2000 = value;
			}
		}

		internal double Dec2000
		{
			get
			{
				return dec2000;
			}
			set
			{
				dec2000 = value;
			}
		}

		internal double GrazeLongitude
		{
			get
			{
				return grazeLongitude;
			}
			set
			{
				grazeLongitude = value;
			}
		}

		internal double GrazeLatitude
		{
			get
			{
				return grazeLatitude;
			}
			set
			{
				grazeLatitude = value;
			}
		}

		internal double GrazeLongitude2
		{
			get
			{
				return grazeLongitude2;
			}
			set
			{
				grazeLongitude2 = value;
			}
		}

		internal double GrazeLatitude2
		{
			get
			{
				return grazeLatitude2;
			}
			set
			{
				grazeLatitude2 = value;
			}
		}

		internal double GrazeD
		{
			get
			{
				return grazeD;
			}
			set
			{
				grazeD = value;
			}
		}

		internal double GrazeAZ
		{
			get
			{
				return grazeAZ;
			}
			set
			{
				grazeAZ = value;
			}
		}

		internal double GrazeDeltaLat
		{
			get
			{
				return grazeDeltaLat;
			}
			set
			{
				grazeDeltaLat = value;
			}
		}

		internal int GrazeInnerOuterLimit
		{
			get
			{
				return grazeInnerOuterLimit;
			}
			set
			{
				grazeInnerOuterLimit = value;
			}
		}

		internal int ElementRecordNumber
		{
			get
			{
				return elementRecordNumber;
			}
			set
			{
				elementRecordNumber = value;
			}
		}

		internal double N
		{
			get
			{
				return n;
			}
			set
			{
				n = value;
			}
		}

		internal bool EclipseEvent
		{
			get
			{
				return eclipseEvent;
			}
			set
			{
				eclipseEvent = value;
			}
		}

		internal bool ZCNameExists => starNameExists;

		internal bool Kepler2WantedExists => kepler2WantedExists;

		internal bool DoubleDetailsExist => doubleDetailsExist;

		internal bool DoubleWantedExists => doubleWantedExists;

		internal bool VariableDetailsExist => variableDetailsExist;

		internal bool LightCurveAvailable
		{
			get
			{
				return lightCurveAvailable;
			}
			set
			{
				lightCurveAvailable = value;
			}
		}

		internal int Tag
		{
			get
			{
				return tag;
			}
			set
			{
				tag = value;
			}
		}

		public int CompareTo(object other)
		{
			return JD.CompareTo(((LunarPredictionLine)other).JD);
		}

		public override string ToString()
		{
			return FormatLine(ShortLine: false);
		}

		public string FormatLine(bool ShortLine)
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (IsGrazeLine)
			{
				if (Settings.Default.Grazes_DMScoords)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(GrazeLongitude, 4, 0, MinutesOnly: false));
					stringBuilder.Append("  " + Utilities.DEGtoDMS(GrazeLatitude, 3, 1, MinutesOnly: false));
				}
				else if (Settings.Default.Grazes_DDDcoords)
				{
					if (GrazeLongitude < 0.0)
					{
						stringBuilder.Append("-");
					}
					else
					{
						stringBuilder.Append(" ");
					}
					stringBuilder.AppendFormat("{0,8:F4} ", Math.Abs(GrazeLongitude));
					if (GrazeLatitude < 0.0)
					{
						stringBuilder.Append("  -");
					}
					else
					{
						stringBuilder.Append("   ");
					}
					stringBuilder.AppendFormat("{0,8:F5} ", Math.Abs(GrazeLatitude));
				}
				else
				{
					stringBuilder.Append(Utilities.DEGtoDMS(GrazeLongitude, 4, 2, MinutesOnly: true));
					stringBuilder.Append("  " + Utilities.DEGtoDMS(GrazeLatitude, 3, 3, MinutesOnly: true));
				}
				double num = Math.Floor(jD - 0.5) + 0.5;
				double num2 = jD - num;
				stringBuilder.Append(" " + Utilities.DEGtoDMS(num2 * 24.0, 3, 0, MinutesOnly: false));
				if (moonAlt >= 0.0)
				{
					if (sunAlt > -12.0)
					{
						stringBuilder.AppendFormat("{0,5:F0}", sunAlt);
					}
					else
					{
						stringBuilder.Append("     ");
					}
					stringBuilder.AppendFormat("{0,4:F0}", moonAlt);
					stringBuilder.AppendFormat("{0,4:F0}", moonAz);
					double num3 = 1.0 / Math.Tan(moonAlt / (180.0 / Math.PI));
					if (num3 > 99.0)
					{
						num3 = 99.0;
					}
					stringBuilder.AppendFormat("{0,6:F2}", num3);
					stringBuilder.AppendFormat("{0,6:F1}", PA);
					stringBuilder.AppendFormat("{0,7:F2}", aA);
					stringBuilder.AppendFormat("{0,7:F2}", cA);
					stringBuilder.Append(cusp);
				}
				else
				{
					stringBuilder.Append("  Moon below horizon");
				}
			}
			else
			{
				double num4 = Math.Floor(jD - 0.5) + 0.5;
				double num5 = jD - num4;
				stringBuilder.Append(Utilities.Date_from_JD(num4, 0).Substring(2));
				if ((Math.Abs(rV) < 0.1) | !Utilities.LOLAFileExists)
				{
					stringBuilder.Append(Utilities.DEGtoDMS(num5 * 24.0, 3, 0, MinutesOnly: false) + "  ");
				}
				else
				{
					stringBuilder.Append(Utilities.DEGtoDMS(num5 * 24.0, 3, 1, MinutesOnly: false));
				}
				stringBuilder.Append(" " + eventPhase);
				stringBuilder.Append(starID.PadRight(10).Substring(0, 10));
				stringBuilder.AppendFormat("{0,5:F1}", mv);
				if (LightCurveAvailable)
				{
					stringBuilder.AppendFormat("*");
				}
				else
				{
					stringBuilder.Append(" ");
				}
				if (mr == 0.0)
				{
					stringBuilder.AppendFormat("    ");
				}
				else
				{
					stringBuilder.AppendFormat("{0,4:F1}", mr);
				}
				stringBuilder.Append(variableCode);
				stringBuilder.AppendFormat("{0,4:F0}", illumination);
				stringBuilder.Append(waxFlag);
				stringBuilder.AppendFormat("{0,4:F0}", elongation);
				if (sunAlt > -12.0)
				{
					stringBuilder.AppendFormat("{0,4:F0}", sunAlt);
				}
				else
				{
					stringBuilder.Append("    ");
				}
				stringBuilder.AppendFormat("{0,3:F0}", moonAlt);
				if (grazeLatitude2 < 100.0)
				{
					stringBuilder.AppendFormat(" ** GRAZE: CA{0,5:F1}", CA);
					stringBuilder.Append(cusp);
					stringBuilder.AppendFormat("; Dist.{0,3:F0}km in az. {1,3:F0}deg.", grazeD, grazeAZ);
					stringBuilder.AppendFormat(" [Lat ={0,6:F2}", grazeLatitude);
					if (grazeDeltaLat >= 0.0)
					{
						stringBuilder.AppendFormat("+");
					}
					else
					{
						stringBuilder.AppendFormat("-");
					}
					stringBuilder.AppendFormat("{0,1:F2}(E.Long", Math.Abs(grazeDeltaLat));
					if (grazeLongitude <= 0.0)
					{
						stringBuilder.AppendFormat("+");
					}
					stringBuilder.AppendFormat("{0,1:F2})]", 0.0 - grazeLongitude);
				}
				else
				{
					stringBuilder.AppendFormat("{0,4:F0}", moonAz);
					stringBuilder.AppendFormat("{0,4:F0}", cA);
					stringBuilder.Append(cusp);
					stringBuilder.AppendFormat("{0,4:F0}", pA);
					stringBuilder.AppendFormat("{0,4:F0}", vA);
					stringBuilder.AppendFormat("{0,4:F0}", aA);
					if (!ShortLine)
					{
						stringBuilder.AppendFormat(" {0:+0.0;-0.0}", lL);
						stringBuilder.AppendFormat(" {0:+0.0;-0.0}", lB);
						stringBuilder.AppendFormat(" {0:+0.0;-0.0}", a);
						stringBuilder.AppendFormat("{0:+0.0;-0.0}", b);
						stringBuilder.AppendFormat("{0: .000}", rV);
						stringBuilder.AppendFormat("{0,7:F1}", cCT);
						if ((mv != 0.0) & (mp != 0.0) & (rV > 0.005) & (Planet == 0))
						{
							double num6 = Math.Pow(10.0, 0.73 * (mp - mv) - 0.2 * mv - 2.512) / rV;
							if (num6 > 0.01)
							{
								stringBuilder.AppendFormat("{0: .00}", num6);
							}
							else
							{
								stringBuilder.Append("    ");
							}
						}
						else
						{
							stringBuilder.Append("    ");
						}
						if (!Settings.Default.LunarPredict_DisplayApparentPosition)
						{
							stringBuilder.Append(" " + Utilities.DEGtoDMS(rA2000 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false));
							stringBuilder.Append(" " + Utilities.DEGtoDMS(dec2000 * (180.0 / Math.PI), 3, 0, MinutesOnly: false));
						}
						else
						{
							stringBuilder.Append(" " + Utilities.DEGtoDMS(rA * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false));
							stringBuilder.Append(" " + Utilities.DEGtoDMS(dec * (180.0 / Math.PI), 3, 0, MinutesOnly: false));
						}
						stringBuilder.AppendFormat(" {0,5:f1}", moonDistanceKM / 1000.0);
						stringBuilder.AppendFormat("{0,6:f1}", moonSpeedMsec);
					}
				}
			}
			return stringBuilder.ToString();
		}

		public string BAAPrediction(bool UseCA)
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			double num2 = jD - num;
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.Date_from_JD(num, 0).Substring(2));
			stringBuilder.Append(starID.PadRight(10).Substring(0, 7));
			stringBuilder.AppendFormat("{0,5:F1}", mv);
			if (EventPhase.ToUpper().Contains("G"))
			{
				stringBuilder.Append(" G");
			}
			else if (EventPhase.ToUpper().Contains("D"))
			{
				stringBuilder.Append(" D");
			}
			else if (EventPhase.ToUpper().Contains("M"))
			{
				stringBuilder.Append(" M");
			}
			else
			{
				stringBuilder.Append(" R");
			}
			if (CA < 0.0)
			{
				stringBuilder.Append("B");
			}
			else
			{
				stringBuilder.Append("D");
			}
			if (UseCA)
			{
				stringBuilder.Append(string.Format("{0,4:F0}", Illumination) + waxFlag);
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0} ", elongation);
			}
			stringBuilder.Append(Utilities.DEGtoDMS(num2 * 24.0, 3, 1, MinutesOnly: true));
			stringBuilder.AppendFormat(" {0:+0.0;-0.0}", a);
			stringBuilder.AppendFormat(" {0:+0.0;-0.0}", b);
			if (UseCA)
			{
				stringBuilder.AppendFormat("{0,4:F0}", cA);
				stringBuilder.Append(cusp);
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0}", pA);
			}
			return stringBuilder.ToString();
		}

		public string LongEventPrediction()
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			double num2 = jD - num;
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.DEGtoDMS(num2 * 24.0, 3, 0, MinutesOnly: false));
			if (sunAlt > -12.0)
			{
				stringBuilder.AppendFormat("{0,4:F0}", sunAlt);
			}
			else
			{
				stringBuilder.Append("    ");
			}
			stringBuilder.AppendFormat("{0,3:F0}", moonAlt);
			stringBuilder.AppendFormat("{0,4:F0}", moonAz);
			stringBuilder.AppendFormat("{0,4:F0}", cA);
			stringBuilder.Append(cusp);
			stringBuilder.AppendFormat("{0,4:F0}", pA);
			stringBuilder.AppendFormat("{0,4:F0}", aA);
			stringBuilder.AppendFormat(" {0:+0.0;-0.0}", a);
			stringBuilder.AppendFormat(" {0:+0.0;-0.0}", b);
			return stringBuilder.ToString();
		}

		public string ShortEventPrediction()
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			double num2 = jD - num;
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.DEGtoDMS(num2 * 24.0, 3, 0, MinutesOnly: false));
			if (sunAlt > -12.0)
			{
				stringBuilder.AppendFormat("{0,4:F0}", sunAlt);
			}
			else
			{
				stringBuilder.Append("    ");
			}
			stringBuilder.AppendFormat("{0,3:F0}", moonAlt);
			stringBuilder.AppendFormat("{0,4:F0}", cA);
			stringBuilder.Append(cusp);
			stringBuilder.AppendFormat("{0,4:F0}", pA);
			return stringBuilder.ToString();
		}

		public string GrazeSummmaryLine()
		{
			StringBuilder stringBuilder = new StringBuilder();
			double num = Math.Floor(jD - 0.5) + 0.5;
			double num2 = jD - num;
			stringBuilder.Append(Utilities.Date_from_JD(num, 0).Substring(2));
			stringBuilder.Append(Utilities.DEGtoDMS(num2 * 24.0, 3, 0, MinutesOnly: false));
			stringBuilder.Append("   ");
			stringBuilder.Append(starID);
			stringBuilder.AppendFormat("{0,5:F1}", mv);
			if (mr == 0.0)
			{
				stringBuilder.AppendFormat("     ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,5:F1}", mr);
			}
			stringBuilder.Append(variableCode);
			stringBuilder.AppendFormat("{0,4:F0}", illumination);
			stringBuilder.Append(waxFlag);
			stringBuilder.AppendFormat("{0,4:F0}", elongation);
			if (sunAlt > -12.0)
			{
				stringBuilder.AppendFormat("{0,4:F0}", sunAlt);
			}
			else
			{
				stringBuilder.Append("    ");
			}
			stringBuilder.AppendFormat("{0,3:F0}", moonAlt);
			stringBuilder.AppendFormat("{0,4:F0}", moonAz);
			stringBuilder.AppendFormat("{0,9:F1}", CA);
			stringBuilder.Append(cusp);
			stringBuilder.AppendFormat("{0,10:F0}", grazeD);
			return stringBuilder.ToString();
		}

		public string GrazeMapSummmaryLine()
		{
			StringBuilder stringBuilder = new StringBuilder();
			double num = Math.Floor(jD - 0.5) + 0.5;
			double num2 = jD - num;
			stringBuilder.Append(Utilities.Date_from_JD(num, 0).Substring(2));
			stringBuilder.Append(Utilities.DEGtoDMS(num2 * 24.0, 3, 0, MinutesOnly: true));
			stringBuilder.Append("   ");
			if (!int.TryParse(starID.Trim().PadRight(3).Substring(0, 2), out var _))
			{
				stringBuilder.Append(starID.Trim().PadRight(10).Substring(0, 7));
			}
			else
			{
				stringBuilder.Append(starID.PadLeft(10).Substring(0, 7));
			}
			stringBuilder.AppendFormat("{0,5:F1}", mv);
			if (mr == 0.0)
			{
				stringBuilder.AppendFormat("     ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,5:F1}", mr);
			}
			stringBuilder.Append(variableCode);
			stringBuilder.AppendFormat("{0,4:F0}", illumination);
			stringBuilder.Append(waxFlag);
			stringBuilder.AppendFormat("{0,6:F0}", elongation);
			stringBuilder.AppendFormat("{0,9:F1}", CA);
			stringBuilder.Append(cusp);
			if (sunAlt > -12.0)
			{
				stringBuilder.AppendFormat("{0,4:F0}", sunAlt);
			}
			else
			{
				stringBuilder.Append("    ");
			}
			return stringBuilder.ToString();
		}

		public string EventDate()
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			_ = jD;
			return Utilities.Date_from_JD(num, 0);
		}

		public void EventDate(out int year, out int month, out int day)
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			_ = jD;
			Utilities.Date_from_JD(num, out year, out month, out var day2);
			day = (int)(day2 + 0.0001);
		}

		public string EventTime()
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			return Utilities.DEGtoDMS((jD - num) * 24.0, 3, 1, MinutesOnly: false);
		}

		public double EventUT()
		{
			double num = Math.Floor(jD - 0.5) + 0.5;
			return (jD - num) * 24.0;
		}

		public string EventID()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(eventPhase);
			stringBuilder.Append(starID);
			stringBuilder.AppendFormat("{0,5:F1}", mv);
			stringBuilder.AppendFormat("{0,4:F0}", cA);
			stringBuilder.Append(cusp);
			stringBuilder.AppendFormat("{0,4:F0}", pA);
			return stringBuilder.ToString();
		}
	}
}
