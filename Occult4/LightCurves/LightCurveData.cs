using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Occult;
using Occult.Star_Catalogues;

namespace LightCurves
{
	internal class LightCurveData : IComparable
	{
		private const char Separator = ':';

		private bool excludeLightCurve = true;

		private bool reviewLightCurve;

		private bool isMoon;

		private bool isAsteroid;

		private bool noStarNumberExpected;

		private int numPoints;

		private int year = 1900;

		private int month;

		private int day;

		private int hr;

		private int min;

		private double duration;

		private double sec;

		private double interval;

		private double jD;

		internal static AsteroidElements_All Asteroids = new AsteroidElements_All(Utilities.AppPath + "\\Resource Files\\AsteroidElements.csv");

		private short tyc2A;

		private short tyc2B;

		private short tyc2C = 1;

		private int hipparcos;

		private int sAO;

		private int xZ;

		private int zc;

		private int u4zone;

		private int u4num;

		private int b1Zone;

		private int b1Number;

		private int n1Zone;

		private int n1Number;

		private long kepler2;

		private string starForFileName = "";

		private string eW = "+";

		private string nS = "+";

		private string observer = "";

		private int longD;

		private int longM;

		private int latD;

		private int latM;

		private int altM;

		private double longS;

		private double latS;

		private double observerLongitude;

		private double observerLatitude;

		private double axisAngle;

		private double lib_L;

		private double lib_B;

		private double limbSlope;

		private double normalMotion;

		private double contactAngle;

		private double moonSize = 100.0;

		private double posAngle;

		private int illumination;

		private int moonALt;

		private string cuspAngle = "0N";

		private int asteroidNumber = -1;

		private string asteroidName = "";

		private List<int> lightValues = new List<int>();

		private List<bool> lightValuesValid = new List<bool>();

		private string fileName = "";

		public string FileName
		{
			get
			{
				return fileName;
			}
			set
			{
				fileName = value;
			}
		}

		public bool ExcludeLightCurve
		{
			get
			{
				return excludeLightCurve;
			}
			set
			{
				excludeLightCurve = value;
			}
		}

		public bool ReviewLightCurve
		{
			get
			{
				return reviewLightCurve;
			}
			set
			{
				reviewLightCurve = value;
			}
		}

		public bool IsMoon => isMoon;

		public bool IsAsteroid => isAsteroid;

		public int Year
		{
			get
			{
				return year;
			}
			set
			{
				year = value;
			}
		}

		public int Month
		{
			get
			{
				return month;
			}
			set
			{
				month = value;
			}
		}

		public int Day
		{
			get
			{
				return day;
			}
			set
			{
				day = value;
			}
		}

		public double EventJD => Utilities.JD_from_Date(year, month, day);

		public string EventDate => $"{Year} {Utilities.ShortMonths[Month]} {Day:00}";

		public int Hr
		{
			get
			{
				return hr;
			}
			set
			{
				hr = value;
			}
		}

		public int Min
		{
			get
			{
				return min;
			}
			set
			{
				min = value;
			}
		}

		public double Sec
		{
			get
			{
				return sec;
			}
			set
			{
				sec = value;
			}
		}

		public double StartTimeSecs => (double)(hr * 3600 + min * 60) + sec;

		public double JD => jD;

		public int NumPoints
		{
			get
			{
				return numPoints;
			}
			set
			{
				numPoints = value;
			}
		}

		public string SetHMS
		{
			set
			{
				if (!int.TryParse(value.Substring(0, 2), out hr))
				{
					hr = 0;
				}
				if (!int.TryParse(value.Substring(3, 2), out min))
				{
					min = 0;
				}
				if (!double.TryParse(value.Substring(6), out sec))
				{
					sec = 0.0;
				}
			}
		}

		public double Duration
		{
			get
			{
				return duration;
			}
			set
			{
				duration = value;
			}
		}

		public double Interval
		{
			get
			{
				return interval;
			}
			set
			{
				interval = value;
			}
		}

		public bool NoStarNumberExpected
		{
			get
			{
				return noStarNumberExpected;
			}
			set
			{
				noStarNumberExpected = value;
			}
		}

		public int Hipparcos
		{
			get
			{
				return hipparcos;
			}
			set
			{
				hipparcos = value;
			}
		}

		public int SAO
		{
			get
			{
				return sAO;
			}
			set
			{
				sAO = value;
			}
		}

		public int XZ
		{
			get
			{
				return xZ;
			}
			set
			{
				xZ = value;
				XZ80Q.Get_XZ_Star(xZ);
				zc = XZ80Q.ZC;
			}
		}

		public int ZC => zc;

		public long Kepler2
		{
			get
			{
				return kepler2;
			}
			set
			{
				kepler2 = value;
				if (kepler2 < 0)
				{
					kepler2 = 0L;
				}
			}
		}

		public short Tyc2A
		{
			get
			{
				return tyc2A;
			}
			set
			{
				tyc2A = value;
			}
		}

		public short Tyc2B
		{
			get
			{
				return tyc2B;
			}
			set
			{
				tyc2B = value;
			}
		}

		public short Tyc2C
		{
			get
			{
				return tyc2C;
			}
			set
			{
				tyc2C = value;
			}
		}

		public string SetTycho2
		{
			set
			{
				string[] array = value.Split(new char[1] { '-' });
				tyc2A = short.Parse(array[0]);
				tyc2B = short.Parse(array[1]);
				if ((array.Length > 2) & (array[2].Length > 0))
				{
					tyc2C = short.Parse(array[2]);
				}
			}
		}

		public string StarForFileName
		{
			get
			{
				return starForFileName;
			}
			set
			{
				starForFileName = value;
			}
		}

		public string FileNameForSave
		{
			get
			{
				string text = "";
				if (AsteroidNumber > 0)
				{
					text = "(" + AsteroidNumber + ")_";
				}
				else if (starForFileName.Length > 0)
				{
					text = starForFileName + "_";
				}
				return text + $"{year:0000}{month:00}{day:00}_{hr:00}{min:00}{sec:00.00}".Replace(".", "-") + ".dat";
			}
		}

		public int U4Zone
		{
			get
			{
				return u4zone;
			}
			set
			{
				u4zone = value;
			}
		}

		public int U4Number
		{
			get
			{
				return u4num;
			}
			set
			{
				u4num = value;
			}
		}

		public int B1Zone
		{
			get
			{
				return b1Zone;
			}
			set
			{
				b1Zone = value;
			}
		}

		public int B1Number
		{
			get
			{
				return b1Number;
			}
			set
			{
				b1Number = value;
			}
		}

		public int N1Zone
		{
			get
			{
				return n1Zone;
			}
			set
			{
				n1Zone = value;
			}
		}

		public int N1Number
		{
			get
			{
				return n1Number;
			}
			set
			{
				u4num = n1Number;
			}
		}

		public string Tycho2 => tyc2A + "-" + tyc2B + "-" + tyc2C;

		public string U4 => u4zone + "-" + u4num;

		public string B1 => b1Zone + "-" + b1Number;

		public string N1 => n1Zone + "-" + n1Number;

		public string EW
		{
			get
			{
				return eW;
			}
			set
			{
				eW = value;
			}
		}

		public int LongD
		{
			get
			{
				return longD;
			}
			set
			{
				longD = value;
			}
		}

		public int LongM
		{
			get
			{
				return longM;
			}
			set
			{
				longM = value;
			}
		}

		public double LongS
		{
			get
			{
				return longS;
			}
			set
			{
				longS = value;
			}
		}

		public string Longitude => eW + $"{longD:##0} {longM:00} {longS:00.0}";

		public double ObserverLongitude => observerLongitude;

		public double SetLongitude_fromDeg
		{
			set
			{
				observerLongitude = value;
				double num = value;
				if (num < 0.0)
				{
					eW = "-";
					num = 0.0 - num;
				}
				else
				{
					eW = "+";
				}
				longD = (int)Math.Floor(num);
				double num2 = 60.0 * (num - (double)longD);
				longM = (int)Math.Floor(num2);
				longS = 60.0 * (num2 - (double)longM);
			}
		}

		public string NS
		{
			get
			{
				return nS;
			}
			set
			{
				nS = value;
			}
		}

		public int LatD
		{
			get
			{
				return latD;
			}
			set
			{
				latD = value;
			}
		}

		public int LatM
		{
			get
			{
				return latM;
			}
			set
			{
				latM = value;
			}
		}

		public double LatS
		{
			get
			{
				return latS;
			}
			set
			{
				latS = value;
			}
		}

		public string Latitude => nS + $"{latD:##0} {latM:00 }{latS:00.0} ";

		public double ObserverLatitude => observerLatitude;

		public double SetLatitude_fromDeg
		{
			set
			{
				observerLatitude = value;
				double num = value;
				if (num < 0.0)
				{
					nS = "-";
					num = 0.0 - num;
				}
				else
				{
					nS = "+";
				}
				latD = (int)Math.Floor(num);
				double num2 = 60.0 * (num - (double)latD);
				latM = (int)Math.Floor(num2);
				latS = 60.0 * (num2 - (double)latM);
			}
		}

		public int AltM
		{
			get
			{
				return altM;
			}
			set
			{
				altM = value;
			}
		}

		public string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}

		public double AxisAngle
		{
			get
			{
				return axisAngle;
			}
			set
			{
				axisAngle = value;
			}
		}

		public double Lib_L
		{
			get
			{
				return lib_L;
			}
			set
			{
				lib_L = value;
			}
		}

		public double Lib_B
		{
			get
			{
				return lib_B;
			}
			set
			{
				lib_B = value;
			}
		}

		public double LimbSlope
		{
			get
			{
				return limbSlope;
			}
			set
			{
				limbSlope = value;
			}
		}

		public double NormalMotion
		{
			get
			{
				return normalMotion;
			}
			set
			{
				normalMotion = value;
			}
		}

		public double ContactAngle
		{
			get
			{
				return contactAngle;
			}
			set
			{
				contactAngle = value;
			}
		}

		public double MoonSize
		{
			get
			{
				return moonSize;
			}
			set
			{
				moonSize = value;
			}
		}

		public double PosAngle
		{
			get
			{
				return posAngle;
			}
			set
			{
				posAngle = value;
			}
		}

		public int Illumination
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

		public int MoonALt
		{
			get
			{
				return moonALt;
			}
			set
			{
				moonALt = value;
			}
		}

		public string CuspAngle
		{
			get
			{
				return cuspAngle;
			}
			set
			{
				cuspAngle = value;
			}
		}

		public int CA => int.Parse(cuspAngle.Substring(0, cuspAngle.Length - 1));

		public int AsteroidNumber
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

		public List<int> LightValues
		{
			get
			{
				return lightValues;
			}
			set
			{
				lightValues = value;
			}
		}

		public List<bool> LightValuesValid
		{
			get
			{
				return lightValuesValid;
			}
			set
			{
				lightValuesValid = value;
			}
		}

		public bool ReadObservation(string[] Lines, out string ErrorLine)
		{
			if (Lines.Length < 5)
			{
				ErrorLine = "Old Format";
				return false;
			}
			if (Lines.Length > 5)
			{
				for (int i = 5; i < Lines.Length; i++)
				{
					Lines[4] += Lines[i];
				}
			}
			ReadObservation_NewFormat(Lines[0], Lines[1], Lines[2], Lines[3], Lines[4], out var ValidFileFormat, out ErrorLine);
			return ValidFileFormat;
		}

		public void GetStars(out int Hip, out int SAOn, out int XZ80Q, out int Tyc_1, out int Tyc_2, out int U4_1, out int U4_2)
		{
			Hip = Hipparcos;
			SAOn = SAO;
			XZ80Q = XZ;
			Tyc_1 = Tyc2A;
			Tyc_2 = Tyc2B;
			U4_1 = U4Zone;
			U4_2 = U4Number;
		}

		public void ReadObservation_NewFormat(string Line0, string Line1, string Line2, string Line3, string Line4, out bool ValidFileFormat, out string ErrorLine)
		{
			ValidFileFormat = true;
			ErrorLine = "All";
			string[] array = new string[1] { "" };
			(new string[1])[0] = "";
			(new string[1])[0] = "";
			string[] array2 = new string[1] { "" };
			(new string[1])[0] = "";
			(new string[1])[0] = "";
			string[] array3 = new string[1] { "" };
			string[] array4 = new string[1] { "" };
			string[] array5 = new string[1] { "" };
			if (Line0.Substring(10, 1) == ":")
			{
				Line0 = Line0.Remove(10, 1).Insert(10, "-").Remove(13, 1)
					.Insert(13, "-")
					.Remove(16, 1);
			}
			array = Line0.Replace('\r', ' ').Split(new char[1] { ':' });
			array2 = Line1.Replace('\r', ' ').Split(new char[1] { ':' });
			array3 = Line2.Replace('\r', ' ').Split(new char[1] { ':' });
			array4 = Line3.Replace('\r', ' ').Split(new char[1] { ':' });
			array5 = Line4.Replace('\r', ' ').Split(new char[1] { ':' });
			if ((array.Length < 4) | (array2.Length < 6) | (array3.Length < 6) | (array4.Length < 3) | (array5.Length < 6))
			{
				ErrorLine = "";
				if (array.Length < 4)
				{
					ErrorLine += "- Date line has < 4 fields\r\n";
				}
				if (array2.Length < 7)
				{
					ErrorLine += "- Star line has < 7 fields\r\n";
				}
				if (array3.Length < 9)
				{
					ErrorLine += "- Observer line has < 9 fields\r\n";
				}
				if (array4.Contains("Asteroid"))
				{
					if (array4.Length < 4)
					{
						ErrorLine += "- Object-Asteroid line has <4 fields\r\n";
					}
				}
				else if (array4.Length < 13)
				{
					ErrorLine += "- Object-Moon line has <13 fields\r\n";
				}
				if (array5.Length < 30)
				{
					ErrorLine += "- Values line does not have enough fields for a useful light curve\r\n";
				}
				ValidFileFormat = false;
				return;
			}
			try
			{
				ErrorLine = "DateTime";
				if (array[1].Length > 1)
				{
					string[] array6 = array[1].Split(new char[1] { '-' });
					if (!int.TryParse(array6[0], out year))
					{
						year = 1900;
					}
					if (!int.TryParse(array6[1], out month))
					{
						month = 1;
					}
					string[] array7 = array6[2].Trim().Split(new char[1] { ' ' });
					if (!int.TryParse(array7[0], out day))
					{
						day = 1;
					}
					if (!int.TryParse(array7[1], out hr))
					{
						hr = 0;
					}
				}
				if (!int.TryParse(array[2], out min))
				{
					min = 0;
				}
				if (!double.TryParse(array[3], out sec))
				{
					sec = 0.0;
				}
				jD = Utilities.JD_from_Date(year, month, (double)day + ((double)hr + (double)min / 60.0 + sec / 3600.0) / 24.0);
				if (!double.TryParse(array[4], out duration))
				{
					duration = 0.0;
				}
				if (!int.TryParse(array[5], out numPoints))
				{
					numPoints = 0;
				}
				if (NumPoints > array5.Length - 1)
				{
					ErrorLine = "- Number of points set in Line 1 (" + NumPoints + ") is greater than the actual number of points (" + (array5.Length - 1) + ")\r\n";
					ValidFileFormat = false;
					return;
				}
				if (NumPoints > 1)
				{
					Interval = Duration / (double)(NumPoints - 1);
				}
				else
				{
					Interval = 0.0;
				}
				ErrorLine = "Star";
				if (!int.TryParse(array2[1], out hipparcos))
				{
					hipparcos = 0;
				}
				if (!int.TryParse(array2[2], out sAO))
				{
					sAO = 0;
				}
				if (!int.TryParse(array2[3], out xZ))
				{
					xZ = 0;
				}
				if (xZ > 0)
				{
					XZ80Q.Get_XZ_Star(xZ);
					zc = XZ80Q.ZC;
				}
				if (!long.TryParse(array2[4], out kepler2))
				{
					kepler2 = 0L;
				}
				if (array2[5].Length > 1)
				{
					string[] array8 = array2[5].Split(new char[1] { '-' });
					if (!short.TryParse(array8[0], out tyc2A))
					{
						tyc2A = 0;
					}
					if (!short.TryParse(array8[1], out tyc2B))
					{
						tyc2B = 0;
					}
					if (!short.TryParse(array8[2], out tyc2C))
					{
						tyc2C = 1;
					}
				}
				int num = array2.Length;
				u4zone = (u4num = 0);
				if (array2[num - 1].Length > 1)
				{
					string[] array9 = array2[num - 1].Split(new char[1] { '-' });
					if (!int.TryParse(array9[0], out u4zone))
					{
						u4zone = 0;
					}
					if (!int.TryParse(array9[1], out u4num))
					{
						u4num = 0;
					}
				}
				ErrorLine = "Observer";
				if (array3[1].Contains("-"))
				{
					eW = "-";
				}
				if (!int.TryParse(array3[1].Replace("-", ""), out longD))
				{
					longD = 0;
				}
				if (!int.TryParse(array3[2], out longM))
				{
					longM = 0;
				}
				if (!double.TryParse(array3[3], out longS))
				{
					longS = 0.0;
				}
				if (array3[4].Contains("-"))
				{
					nS = "-";
				}
				if (!int.TryParse(array3[4].Replace("-", ""), out latD))
				{
					latD = 0;
				}
				if (!int.TryParse(array3[5], out latM))
				{
					latM = 0;
				}
				if (!double.TryParse(array3[6], out latS))
				{
					latS = 0.0;
				}
				if (!int.TryParse(array3[7], out altM))
				{
					altM = 0;
				}
				Observer = array3[8].Trim();
				ErrorLine = "MoonOrAsteroid";
				if (array4[1].Trim() == "Moon")
				{
					isMoon = true;
					if (!double.TryParse(array4[2], out axisAngle))
					{
						axisAngle = 0.0;
					}
					if (!double.TryParse(array4[3], out lib_L))
					{
						lib_L = 0.0;
					}
					if (!double.TryParse(array4[4], out lib_B))
					{
						lib_B = 0.0;
					}
					if (!double.TryParse(array4[5], out limbSlope))
					{
						limbSlope = 0.0;
					}
					if (!double.TryParse(array4[6], out normalMotion))
					{
						normalMotion = 0.0;
					}
					if (!double.TryParse(array4[7], out contactAngle))
					{
						contactAngle = 0.0;
					}
					if (!double.TryParse(array4[8], out moonSize))
					{
						moonSize = 0.0;
					}
					if (!double.TryParse(array4[9], out posAngle))
					{
						posAngle = 0.0;
					}
					CuspAngle = array4[10].PadRight(1).Substring(1);
					if (!int.TryParse(array4[11], out illumination))
					{
						illumination = 0;
					}
					if (!int.TryParse(array4[12], out moonALt))
					{
						moonALt = 0;
					}
					AsteroidName = "";
					AsteroidNumber = 0;
				}
				else
				{
					isAsteroid = true;
					if (!int.TryParse(array4[2], out asteroidNumber))
					{
						asteroidNumber = 0;
					}
					asteroidName = array4[3].Trim();
					moonSize = 100.0;
					axisAngle = (lib_L = (lib_B = (limbSlope = (normalMotion = (contactAngle = (posAngle = 0.0))))));
					illumination = (moonALt = 0);
					cuspAngle = "0N";
				}
				ErrorLine = "Data";
				LightValues = new List<int>();
				LightValues.Clear();
				for (int i = 1; i < array5.Length; i++)
				{
					LightValuesValid.Add(int.TryParse(array5[i], out var result));
					LightValues.Add(result);
				}
			}
			catch
			{
				ValidFileFormat = false;
			}
		}

		public string DateDetails()
		{
			string[] obj = new string[7]
			{
				string.Format("     Date: {0} {1} {2,2}", Year, Utilities.ShortMonths[Month], Day),
				"\r\n",
				string.Format(" Start at: {0,2}h {1,2}m {2,5:f2}s UT", Hr, Min, Sec),
				"\r\n",
				string.Format(" Duration: {0,1:f2} secs\r\n", Duration),
				string.Format(" # points: {0,1:f0}\r\n", NumPoints),
				null
			};
			double num2 = (Interval = Duration / (double)(NumPoints - 1));
			obj[6] = string.Format(" Interval: {0,1:f4} secs\r\n", num2);
			return string.Concat(obj);
		}

		public string StarDetails()
		{
			string text = "";
			if (Hipparcos > 0)
			{
				text = text + "  HIP " + Hipparcos + "\r\n";
			}
			if (SAO > 0)
			{
				text = text + "  SAO " + SAO + "\r\n";
			}
			if (Tyc2B > 0)
			{
				text = text + "  TYC " + Tycho2.ToString() + "\r\n";
			}
			if (XZ > 0)
			{
				text = text + "   XZ " + XZ + "\r\n";
			}
			if (U4Number > 0)
			{
				text = text + "4UCAC " + U4 + "\r\n";
			}
			if (NoStarNumberExpected)
			{
				text += "J, USNO-B1 Or NOMAD, no number required\r\n";
			}
			return text;
		}

		public bool StarCoords_2000(out double RA, out double Dec)
		{
			double Parallax = (RA = (Dec = 0.0));
			bool flag = false;
			double MagR;
			double MagB;
			double MagV;
			double pmDec;
			double pmRA;
			if (!flag & (XZ > 0))
			{
				flag = GetStarPosition.GetXZPosition(XZ, out RA, out Dec, out Parallax, out MagR, out MagB, out MagV, out pmDec, out pmRA);
			}
			bool UsedGaia;
			double pmRA2;
			if (!flag & (U4Number > 0))
			{
				flag = GetStarPosition.GetUCAC4Position(U4Zone, U4Number, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out pmRA2);
			}
			if (!flag & (Hipparcos > 0))
			{
				flag = GetStarPosition.GetHipparcosPosition(Hipparcos, out RA, out Dec, out pmRA2, out Parallax, out MagR, out MagB, out MagV, out pmDec, out pmRA, out UsedGaia, out var _);
			}
			if (!flag & (Tyc2B > 0))
			{
				flag = GetStarPosition.GetTycho2Position(Tyc2A, Tyc2B, Tyc2C, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out pmRA2);
			}
			return flag;
		}

		public string ObserverDetails()
		{
			return " Observer: " + Observer + "\r\nLongitude: " + Longitude + "\r\n Latitude: " + Latitude + "\r\n Altitude: " + AltM + "m\r\n";
		}

		public string Circumstances()
		{
			string text = "";
			if (LightValues.Count < 1)
			{
				text = "*** No light curve data ***\r\n\r\n";
			}
			if (MoonSize < 10.0)
			{
				return text + string.Format("    Axis angle: {0,7:f3}°", AxisAngle) + "\r\n" + string.Format("   Libration l: {0,6:+0.00;-0.00}°", Lib_L) + "\r\n" + string.Format("   Libration b: {0,6:+0.00;-0.00}°", Lib_B) + "\r\n" + string.Format("    Limb slope: {0,6:+0.00;-0.00}°", LimbSlope) + "\r\n" + string.Format(" Normal motion:   {0,6:f4}", NormalMotion) + "\"/sec\r\n" + string.Format(" Contact angle: {0,6:+0.00;-0.00}°", ContactAngle) + "\r\n" + string.Format("     Moon size:  {0,6:f3}", MoonSize) + "\r\n" + string.Format("Position angle: {0,6:f2}°", PosAngle) + "\r\n    Cusp angle: " + CuspAngle + "°\r\n" + string.Format("  illumination: {0,3:f0}%", Illumination) + "\r\n" + string.Format(" Moon altitude:  {0,2:f0}°", MoonALt);
			}
			return text + "Asteroid: (" + AsteroidNumber + ") " + AsteroidName.Trim();
		}

		public bool ErrorCheck()
		{
			bool result = false;
			if ((Hr == 0) & (Min == 0) & (Sec == 0.0))
			{
				result = true;
			}
			if (Duration == 0.0)
			{
				result = true;
			}
			if (NumPoints == 0)
			{
				result = true;
			}
			if (Duration == 0.0)
			{
				result = true;
			}
			if ((LongD == 0) & (LongM == 0) & (LongS == 0.0))
			{
				result = true;
			}
			if ((LatD == 0) & (LatM == 0) & (LatS == 0.0))
			{
				result = true;
			}
			if (Observer.Trim().Length < 1)
			{
				result = true;
			}
			double RA;
			double Dec;
			if (MoonSize < 2.0)
			{
				if (NormalMotion == 0.0)
				{
					result = true;
				}
				if (!StarCoords_2000(out RA, out Dec))
				{
					result = true;
				}
			}
			else
			{
				bool flag = false;
				if (((Hipparcos > 0) | (Tyc2B > 0) | (U4Number > 0) | (XZ > 0)) && !StarCoords_2000(out Dec, out RA))
				{
					result = true;
				}
				if (AsteroidNumber == 0)
				{
					for (int i = 0; i < Utilities.SatelliteNames_All.Length; i++)
					{
						if (AsteroidName.Trim().ToLower() == Utilities.SatelliteNames_All[i].ToLower())
						{
							flag = true;
							break;
						}
					}
					if (!flag)
					{
						for (int j = 0; j < Utilities.Planets.Length; j++)
						{
							if (AsteroidName.Trim().ToLower() == Utilities.Planets[j].ToLower())
							{
								flag = true;
							}
						}
					}
					if (!flag)
					{
						result = true;
					}
				}
				if (AsteroidName.Trim().Length < 2)
				{
					result = true;
				}
				if (AsteroidNumber > 0 && AsteroidName.Trim() != Asteroids.GetAsteroidName_fromNumber(AsteroidNumber, out RA))
				{
					result = true;
				}
			}
			return result;
		}

		public int CompareTo(object other)
		{
			if (JD == ((LightCurveData)other).JD)
			{
				return XZ.CompareTo(((LightCurveData)other).XZ);
			}
			return JD.CompareTo(((LightCurveData)other).JD);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Date:");
			stringBuilder.AppendFormat(" {0:f0}-", Year);
			stringBuilder.AppendFormat("{0:f0}-", Month);
			stringBuilder.AppendFormat("{0:f0} ", Day);
			stringBuilder.AppendFormat("{0:f0}:", Hr);
			stringBuilder.AppendFormat("{0:f0}:", Min);
			stringBuilder.AppendFormat("{0:f2}:", Sec);
			stringBuilder.AppendFormat(" {0:f2}:", Duration);
			stringBuilder.AppendFormat(" {0:f0}\r\n", NumPoints);
			stringBuilder.Append("Star:");
			stringBuilder.AppendFormat(" {0:f0}:", Hipparcos);
			stringBuilder.AppendFormat(" {0:f0}:", SAO);
			stringBuilder.AppendFormat(" {0:f0}:", XZ);
			stringBuilder.AppendFormat(" {0:f0}:", Kepler2);
			stringBuilder.AppendFormat(" {0:f0}-{1:f0}-{2:f0}:", Tyc2A, Tyc2B, Tyc2C);
			stringBuilder.AppendFormat(" {0:f0}-{1:f0}\r\n", U4Zone, U4Number);
			stringBuilder.Append("Observer:");
			stringBuilder.AppendFormat(" {0}{1:f0}:{2:f0}:{3:f1}:", EW, LongD, LongM, LongS);
			stringBuilder.AppendFormat(" {0}{1:f0}:{2:f0}:{3:f1}:", NS, LatD, LatM, LatS);
			stringBuilder.AppendFormat(" {0:f0}:", AltM);
			stringBuilder.Append(" " + Observer + "\r\n");
			stringBuilder.Append("Object:");
			if (moonSize < 2.0)
			{
				stringBuilder.Append(" Moon:");
				stringBuilder.AppendFormat(" {0:f3}:", AxisAngle);
				stringBuilder.AppendFormat(" {0:f3}:", Lib_L);
				stringBuilder.AppendFormat(" {0:f3}:", Lib_B);
				stringBuilder.AppendFormat(" {0:f2}:", LimbSlope);
				stringBuilder.AppendFormat(" {0:f4}:", NormalMotion);
				stringBuilder.AppendFormat(" {0:f2}:", ContactAngle);
				stringBuilder.AppendFormat(" {0:f3}:", MoonSize);
				stringBuilder.AppendFormat(" {0:f2}:", PosAngle);
				stringBuilder.AppendFormat(" {0}:", CuspAngle);
				stringBuilder.AppendFormat(" {0}:", Illumination);
				stringBuilder.AppendFormat(" {0}\r\n", MoonALt);
			}
			else
			{
				stringBuilder.Append(" Asteroid:");
				stringBuilder.AppendFormat(" {0}:", AsteroidNumber);
				stringBuilder.Append(" " + AsteroidName + "\r\n");
			}
			stringBuilder.Append("Values:");
			if (LightValues.Count > 0)
			{
				for (int i = 0; i < LightValues.Count - 1; i++)
				{
					if (LightValuesValid[i])
					{
						stringBuilder.AppendFormat("{0}:", LightValues[i].ToString());
					}
					else
					{
						stringBuilder.Append(':');
					}
				}
				if (LightValuesValid[LightValues.Count - 1])
				{
					stringBuilder.AppendFormat("{0}\r\n", LightValues[LightValues.Count - 1].ToString());
				}
				else
				{
					stringBuilder.Append("\r\n");
				}
			}
			else
			{
				stringBuilder.AppendFormat("\r\n");
			}
			return stringBuilder.ToString();
		}
	}
}
