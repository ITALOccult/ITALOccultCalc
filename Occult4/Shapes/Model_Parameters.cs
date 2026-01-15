using System;

namespace Shapes
{
	internal class Model_Parameters : IComparable
	{
		private string modelNumber_String = "";

		private string lambda = "";

		private string beta = "";

		private string period = "";

		private string yorp = "";

		private string jd0 = "";

		private string phi0 = "";

		private string quality = "";

		private string version = "";

		private string comment = "";

		private string created = "";

		private string modified = "";

		private string asteroidID = "";

		private int asteroidNumber;

		private int modelNumber_Numeric;

		private int isamModelNumber_Numeric;

		private int faces;

		private int vertices;

		private double volumeEquivalentRadius = 0.5;

		private double surfaceEquivalentRadius = 0.5;

		internal static int Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM;

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

		public string ModelNumber_String
		{
			get
			{
				return modelNumber_String;
			}
			set
			{
				modelNumber_String = value;
				int.TryParse(value, out modelNumber_Numeric);
				string[] array = modelNumber_String.Split(new char[1] { '-' });
				if (array.Length > 1)
				{
					isamModelNumber_Numeric = int.Parse(array[1]);
				}
			}
		}

		public int ModelNumber_Numeric => modelNumber_Numeric;

		public int IsamModelNumber_Numeric => isamModelNumber_Numeric;

		public string Lambda
		{
			get
			{
				return lambda;
			}
			set
			{
				lambda = value;
			}
		}

		public string Beta
		{
			get
			{
				return beta;
			}
			set
			{
				beta = value;
			}
		}

		public string Period
		{
			get
			{
				return period;
			}
			set
			{
				period = value;
			}
		}

		public double Period_hrs
		{
			get
			{
				if (period.Length < 1)
				{
					return 0.0;
				}
				return double.Parse(period);
			}
		}

		public double DailyRotationRate
		{
			get
			{
				if (period.Length < 1)
				{
					return 0.0;
				}
				return 8640.0 / double.Parse(Period);
			}
		}

		public string YORP
		{
			get
			{
				return yorp;
			}
			set
			{
				yorp = value;
			}
		}

		public string JD0
		{
			get
			{
				return jd0;
			}
			set
			{
				jd0 = value;
			}
		}

		public double JDat0
		{
			get
			{
				if (jd0.Length < 1)
				{
					return 0.0;
				}
				return double.Parse(jd0);
			}
		}

		public string Phi0
		{
			get
			{
				return phi0;
			}
			set
			{
				phi0 = value;
			}
		}

		public double PhaseAt0
		{
			get
			{
				if (phi0.Length < 1)
				{
					return 0.0;
				}
				return double.Parse(phi0);
			}
		}

		public string Quality
		{
			get
			{
				return quality;
			}
			set
			{
				quality = value;
			}
		}

		public string Version
		{
			get
			{
				return version;
			}
			set
			{
				version = value;
			}
		}

		public string Comment
		{
			get
			{
				return comment;
			}
			set
			{
				comment = value;
			}
		}

		public string VersionComments
		{
			get
			{
				string text = Version.Trim();
				if (Quality.Length > 0)
				{
					if (text.Length > 0)
					{
						text += ", ";
					}
					text = text + "Q=" + Quality;
				}
				if (Comment.Length > 0)
				{
					if (text.Length > 0)
					{
						text += ", ";
					}
					text += Comment.Trim();
				}
				return text;
			}
		}

		public string Created
		{
			get
			{
				return created;
			}
			set
			{
				created = value;
			}
		}

		public string Modified
		{
			get
			{
				return modified;
			}
			set
			{
				modified = value;
			}
		}

		public string DAMIT_Asteroid_ID
		{
			get
			{
				return asteroidID;
			}
			set
			{
				asteroidID = value;
			}
		}

		public double VolumeEquivalentRadius
		{
			get
			{
				return volumeEquivalentRadius;
			}
			set
			{
				volumeEquivalentRadius = value;
			}
		}

		public double SurfaceEquivalentRadius
		{
			get
			{
				return surfaceEquivalentRadius;
			}
			set
			{
				surfaceEquivalentRadius = value;
			}
		}

		public double SurfaceToVolumeRatio
		{
			get
			{
				if (VolumeEquivalentRadius == 0.0)
				{
					return 1.0;
				}
				return SurfaceEquivalentRadius / VolumeEquivalentRadius;
			}
		}

		public int Faces
		{
			get
			{
				return faces;
			}
			set
			{
				faces = value;
			}
		}

		public int Vertices
		{
			get
			{
				return vertices;
			}
			set
			{
				vertices = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM == 1)
			{
				return ModelNumber_Numeric.CompareTo(((Model_Parameters)other).ModelNumber_Numeric);
			}
			if (Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM == 2)
			{
				return ModelNumber_String.CompareTo(((Model_Parameters)other).ModelNumber_String);
			}
			if (AsteroidNumber.CompareTo(((Model_Parameters)other).AsteroidNumber) == 0)
			{
				return IsamModelNumber_Numeric.CompareTo(((Model_Parameters)other).IsamModelNumber_Numeric);
			}
			return AsteroidNumber.CompareTo(((Model_Parameters)other).AsteroidNumber);
		}

		public override string ToString()
		{
			return AsteroidNumber + "," + ModelNumber_String + "," + Lambda + "," + Beta + "," + Period + "," + YORP + "," + JD0 + "," + Phi0 + "," + Quality + "," + Version + "," + Comment + "," + Created + "," + Modified + "," + DAMIT_Asteroid_ID;
		}
	}
}
