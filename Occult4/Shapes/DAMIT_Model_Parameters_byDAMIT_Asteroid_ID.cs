using System;

namespace Shapes
{
	internal class DAMIT_Model_Parameters_byDAMIT_Asteroid_ID : IComparable
	{
		private string modelnum = "";

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

		private int astid;

		public int DAMIT_Asteroid_ID
		{
			get
			{
				return astid;
			}
			set
			{
				astid = value;
			}
		}

		public string DAMIT_ModelNumber
		{
			get
			{
				return modelnum;
			}
			set
			{
				modelnum = value;
			}
		}

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

		public int CompareTo(object other)
		{
			if (DAMIT_Asteroid_ID.CompareTo(((DAMIT_Model_Parameters_byDAMIT_Asteroid_ID)other).DAMIT_Asteroid_ID) == 0)
			{
				return DAMIT_ModelNumber.CompareTo(((DAMIT_Model_Parameters_byDAMIT_Asteroid_ID)other).DAMIT_ModelNumber);
			}
			return DAMIT_Asteroid_ID.CompareTo(((DAMIT_Model_Parameters_byDAMIT_Asteroid_ID)other).DAMIT_Asteroid_ID);
		}

		public override string ToString()
		{
			return DAMIT_ModelNumber + "," + Lambda + "," + Beta + "," + Period + "," + YORP + "," + JD0 + "," + Phi0 + "," + Quality + "," + Version + "," + Comment + "," + Created + "," + Modified + "," + DAMIT_Asteroid_ID;
		}
	}
}
